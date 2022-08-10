import ast
import context
import nodeattrs
import nodebuilder
import syntax
import astrewriter
import visitor


class _CommonStateVisitor(visitor.NoopNodeVisitor):

    def __init__(self, ast_context, syntax):
        super().__init__()
        self.ast_context = ast_context
        self.syntax = syntax
        self.assign_visiting_lhs = False
        self.assign_visiting_rhs = False
        self.loop_visiting_lhs = False
        self.loop_visiting_rhs = False

        # needs to be a stack for nested func names, for example:
        # print("foo".startswith("f"))
        self.func_name_stack = []
        self.parent_node_stack = [] # call/attr

    @property
    def visiting_func(self):
        return False if len(self.parent_node_stack) == 0 else isinstance(self.parent_node_stack[-1], ast.Call)


    @property
    def visiting_attr(self):
        return False if len(self.parent_node_stack) == 0 else isinstance(self.parent_node_stack[-1], ast.Attribute)

    # returns the current func name
    def call(self, node, num_children_visited):
        super().call(node, num_children_visited)
        if num_children_visited == 0:
            self.parent_node_stack.append(node)
            #self.visiting_func = True
        elif num_children_visited == 1:
            self.parent_node_stack.pop()
            #self.visiting_func = False
        elif num_children_visited == -1:
            return self.func_name_stack.pop()
        return None

    def assign(self, node, num_children_visited):
        super().assign(node, num_children_visited)
        assert len(node.targets) == 1
        lhs = node.targets[0]
        if isinstance(lhs, ast.Subscript):
            # d["foo"] = blah - special syntax - skip
            # (this same check exists in most visitors)
            pass
        else:
            if num_children_visited == 0:
                assert not self.assign_visiting_lhs
                assert not self.assign_visiting_rhs
                self.assign_visiting_lhs = True
                self.assign_visiting_rhs = False
            elif num_children_visited != -1:
                self.assign_visiting_lhs = False
                self.assign_visiting_rhs = True
            else: # num_children_visited == -1
                self.assign_visiting_rhs = False

    def attr(self, node, num_children_visited):
        if num_children_visited == 0:
            #self.visiting_attr = True
            self.parent_node_stack.append(node)
        if num_children_visited == -1:
            self.func_name_stack.append(node.attr)
            self.parent_node_stack.pop()
            #self.visiting_attr = False

    def loop_for(self, node, num_children_visited):
        super().loop_for(node, num_children_visited)
        if num_children_visited == 0:
            assert not self.loop_visiting_lhs
            assert not self.loop_visiting_rhs
            self.loop_visiting_lhs = True
        elif num_children_visited == 1:
            assert self.loop_visiting_lhs
            assert not self.loop_visiting_rhs
            self.loop_visiting_lhs = False
            self.loop_visiting_rhs = True
        elif num_children_visited == 2:
            assert not self.loop_visiting_lhs
            assert self.loop_visiting_rhs
            self.loop_visiting_rhs = False

    def name(self, node, num_children_visited):
        super().name(node, num_children_visited)
        if self.visiting_func:
            if self.visiting_func:
                self.func_name_stack.append(node.id)


class FuncCallVisitor(_CommonStateVisitor):
    """
    Executes rewrite rules on the AST - this visitor modifies the AST.
    """

    def __init__(self, ast_context, syntax):
        super().__init__(ast_context, syntax)
        self._keep_revisiting = False

    @property
    def leave_early(self):
        # do we actually need this - tests pass without this (?)
        return self._keep_revisiting

    @property
    def should_revisit(self):
        if self._keep_revisiting:
            self._keep_revisiting = False
            return True
        return False

    def assign(self, node, num_children_visited):
        if not hasattr(node, nodeattrs.REWRITTEN_NODE_ATTR):
            super().assign(node, num_children_visited)
            if num_children_visited == -1:
                assert len(node.targets) == 1
                lhs = node.targets[0]
                if isinstance(lhs, ast.Subscript):
                    # Python "add to dict" syntax: d[key] = value -  provide a
                    # rewrite hook at the assigment node level
                    # (similar checks exist in other visitors)
                    # lhs.value: dict instance
                    # lhs.slice: key
                    # node.value: value
                    self._handle_function_call("<>_dict_assignment", lhs.value, node, arg_nodes=[lhs.slice, node.value])
                else:
                    # use '=' to transform into a function call
                    self._handle_function_call("<>_=", None, node, arg_nodes=[lhs, node.value])

    def binop(self, node, num_children_visited):
        super().binop(node, num_children_visited)
        if num_children_visited == -1:
            if isinstance(node.op, ast.Add):
                op = "+"
            elif isinstance(node.op, ast.Sub):
                op = "-"
            elif isinstance(node.op, ast.Div):
                op = "/"
            elif isinstance(node.op, ast.Mult):
                op = "*"
            else:
                assert False, "Unhandled binop %s" % node.op
            self._handle_function_call("<>_%s" % op, None, node, [node.left, node.right])

    def compare(self, node, num_children_visited):
        super().compare(node, num_children_visited)
        if num_children_visited == -1:
            assert len(node.ops) == 1
            assert len(node.comparators) == 1
            if isinstance(node.ops[0], ast.Eq):
                op = "<>_=="
            else:
                assert False, "Unhandled comparison %s" % node.ops[0]
            self._handle_function_call(op, None, node, [node.left, node.comparators[0]])

    def call(self, node, num_children_visited):
        func_name = super().call(node, num_children_visited)
        if num_children_visited == -1:
            assert func_name is not None
            target_node = None
            if isinstance(node.func, ast.Attribute):
                target_node = node.func.value
            self._handle_function_call(func_name, target_node, node, node.args)

    def cond_if(self, node, num_children_visited, is_expr):
        super().cond_if(node, num_children_visited, is_expr)
        if num_children_visited == -1:
            # we'll pretend this is a function call so we have a rewrite hook
            self._handle_function_call("<>_if", None, node, arg_nodes=[node.test])

    def loop_for(self, node, num_children_visited):
        super().loop_for(node, num_children_visited)
        if num_children_visited == -1:
            self._handle_function_call("<>_loop_for", None, node, arg_nodes=[node.target, node.iter])

    def subscript(self, node, num_children_visited):
        super().subscript(node, num_children_visited)
        target_node = node.value
        target_type = self.ast_context.lookup_type_info_by_node(target_node).value_type
        if num_children_visited == -1:
            if target_type is str:
                assert node.slice.lower is not None, "implement me!"
                arg_nodes = [node.slice.lower]
                if node.slice.upper is not None:
                    arg_nodes.append(node.slice.upper)
            else:
                arg_nodes = [node.slice]
            self._handle_function_call("<>_[]", target_node, node, arg_nodes)

    def funcdef(self, node, num_children_visited):
        super().funcdef(node, num_children_visited)
        if num_children_visited == -1:
            self._handle_function_call("<>_funcdef", None, node, arg_nodes=node.args.args)

    def _handle_function_call(self, func_name, target_node, node, arg_nodes):
        if hasattr(node, nodeattrs.REWRITTEN_NODE_ATTR):
            return
        target_type = None
        if target_node is None:
            if len(arg_nodes) > 0:
                target_type_info = self.ast_context.lookup_type_info_by_node(arg_nodes[0])
                if target_type_info is None:
                    # some nodes do not have a type, for example function
                    # definitions - their type should be "function" by we don't
                    # support those yet
                    pass
                else:
                    target_type = target_type_info.value_type
        else:
            target_type_info = self.ast_context.lookup_type_info_by_node(target_node)
            assert target_type_info is not None, "failed to look up type of target node %s" % target_node
            target_type = target_type_info.value_type
        key = self.syntax.get_function_lookup_key(func_name, target_type)
        if key not in self.syntax.functions:
            key = self.syntax.get_function_lookup_key(func_name, target_type=None)
        if key in self.syntax.functions:
            func = self.syntax.functions[key]
            args = []
            for arg_node in arg_nodes:
                type_info = self.ast_context.lookup_type_info_by_node(arg_node)
                assert type_info is not None, "unable to lookup type info for function %s: arg %s" % (func_name, arg_node)
                args.append(syntax.Argument(arg_node, type_info.value_type))
            rw = astrewriter.ASTRewriter(node, arg_nodes, self.ast_context, target_node)
            # the actual AST rewriting happens here:
            if func.target_name is not None:
                rw.rename(func.target_name)
            if func.function_rewrite is not None:
                func.function_rewrite(args, rw)
            self._keep_revisiting = True
            setattr(node, nodeattrs.REWRITTEN_NODE_ATTR, True)


class TypeVisitor(_CommonStateVisitor):
    """
    This visitor determines the type of every AST Node.
    """

    def __init__(self, ast_context, syntax):
        super().__init__(ast_context, syntax)

        self.lhs_value = None # assignment value "a = b" -> a
        self.lhs_loop_value = None # for loop iter value "for a in l:" -> a
        self.ident_name_to_type_info = {}
        # starts True, set to False when an unresolved type is encountered
        self.resolved_all_type_references = True

        # maps literal node instance to their TypeInfo instance
        self.literal_node_to_type_info = {}

        # emergency brake
        self.num_visits = 0

    @property
    def should_revisit(self):
        if self.resolved_all_type_references:
            return False
        else:
            assert self.num_visits < 6, "cannot resolve all references, check method _assert_resolved_type"
                
            self.resolved_all_type_references = True
            self.num_visits += 1
            return True

    def assign(self, node, num_children_visited):
        super().assign(node, num_children_visited)
        if num_children_visited == -1:
            assert len(node.targets) == 1
            lhs = node.targets[0]
            rhs = node.value
            rhs_type_info = self.ast_context.lookup_type_info_by_node(rhs)
            self._assert_resolved_type(rhs_type_info, "Unable to lookup type of assignment rhs %s" % rhs)
            if isinstance(lhs, ast.Subscript):
                # d["foo"] = blah - special syntax
                # (this same check exists in most visitors)
                # register the type being added to the dict:
                lhs_type_info = self.ast_context.lookup_type_info_by_node(lhs.value)
                self._assert_resolved_type(lhs_type_info, "Unable to lookup type of assignment lhs (subscript) %s" % lhs.value)
                if lhs_type_info is not None:
                    key_type_info = self.ast_context.lookup_type_info_by_node(lhs.slice)
                    lhs_type_info.register_contained_type(0, key_type_info)
                    lhs_type_info.register_contained_type(1, rhs_type_info)
            else:
                # add mapping of lhs id name -> to its type
                self._register_type_info_by_ident_name(self.lhs_value, rhs_type_info)
                self._register_type_info_by_node(lhs, rhs_type_info)

    def subscript(self, node, num_children_visited):
        super().subscript(node, num_children_visited)
        if num_children_visited == -1:
            type_info = self.ast_context.lookup_type_info_by_node(node.value)
            self._assert_resolved_type(type_info, "cannot lookup type of subscript node.value %s" % node.value)
            if type_info is not None:
                if type_info.value_type is str:
                    # str[:] -> str
                    self._register_type_info_by_node(node, type_info)
                else:
                    # container (dict ...)
                    contained_type_info = type_info.get_contained_type_info(is_subscript=True)
                    self._assert_resolved_type(contained_type_info, "cannot lookup contained type of subscript expression %s" % node.value)                
                    self._register_type_info_by_node(node, contained_type_info)

    def attr(self, node, num_children_visited):
        super().attr(node, num_children_visited)
        if num_children_visited == -1:
            # foo.blah() -> the type of foo
            target_instance_type_info = self.ast_context.lookup_type_info_by_node(node.value)
            self._assert_resolved_type(target_instance_type_info, "cannot determine type of target instance %s" % target_instance_type_info)
            if target_instance_type_info is not None:
                func_name = node.attr
                method = self.ast_context.get_method(func_name, target_instance_type_info)
                assert method is not None, "Unknown attr %s" % func_name
                self._register_type_info_by_node(node, method.get_rtn_type_info())

    def unaryop(self, node, num_children_visited):
        super().binop(node, num_children_visited)
        if num_children_visited == -1:
            type_info = self.ast_context.lookup_type_info_by_node(node.operand)
            self._assert_resolved_type(type_info, "unaryop: missing type information for operand %s" % node.operand)
            self._register_type_info_by_node(node, type_info)

    def binop(self, node, num_children_visited):
        super().binop(node, num_children_visited)
        if num_children_visited == -1:
            lhs_type_info = self.ast_context.lookup_type_info_by_node(node.left)
            self._assert_resolved_type(lhs_type_info, "binop: missing type information for lhs %s" % node.left)
            rhs_type_info = self.ast_context.lookup_type_info_by_node(node.right)
            self._assert_resolved_type(rhs_type_info, "binop: missing type information for rhs %s" % node.right)
            if lhs_type_info is not None and rhs_type_info is not None:
                target_type = self.syntax.combine_types(
                    lhs_type_info.value_type, rhs_type_info.value_type)
                target_type_info = context.TypeInfo(target_type)
                self._register_type_info_by_node(node, target_type_info)

    def call(self, node, num_children_visited):
        func_name = super().call(node, num_children_visited)
        if num_children_visited == -1:
            # record this function invocation so we know the argument types
            # it is called with
            # when this visitor runs multiple times, this keeps re-adding the
            # same invocation - dedupe?
            arg_type_infos = [self.ast_context.lookup_type_info_by_node(a) for a in node.args]
            if None in arg_type_infos:
                # not all types have been resolved
                pass
            else:
                # for methods, we should lookup the correct method, based on
                # target_instance_type - since we have not implemented user
                # defined methods, it doesn't matter right now
                func = self.ast_context.get_function(func_name)
                func.register_invocation(arg_type_infos)
                if func.populates_target_instance_container:
                    # in order to understand what type containers store,
                    # we need to track some special method calls
                    # canonical example
                    # l=[]
                    # l.append(1) # <-- this tells us we have a list of ints
                    assert isinstance(node.func, ast.Attribute)
                    # node.func.value: Call.func is an Attribute instance
                    # func.value -> the lhs, dereferenced instance
                    target_instance_type_info = self.ast_context.lookup_type_info_by_node(node.func.value)
                    assert len(arg_type_infos) > 0
                    target_instance_type_info.register_contained_type(0, arg_type_infos[0])

                # propagate the return type from the func child node to this
                # call parent node
                rtn_type_info = self.ast_context.lookup_type_info_by_node(node.func)
                self._assert_resolved_type(rtn_type_info, "no rtn type for func %s %s" % (func.name, node.func))
                self.ast_context.register_type_info_by_node(node, rtn_type_info)

    def cond_if(self, node, num_children_visited, is_expr):
        super().cond_if(node, num_children_visited, is_expr)
        if is_expr:
            if num_children_visited == -1:
                body_type_info = self.ast_context.lookup_type_info_by_node(node.body[0])
                self._assert_resolved_type(body_type_info, "no rtn type for if expr body")
                self._register_type_info_by_node(node, body_type_info)

    def funcdef(self, node, num_children_visited):
        super().funcdef(node, num_children_visited)
        func_name = node.name
        func = self.ast_context.get_function(func_name)
        if num_children_visited == 0:
            # lookup invocations to determine the argument types
            invocation = func.invocations[0] if len(func.invocations) > 0 else None
            self._assert_resolved_type(invocation, "cannot find invocation of function %s" % func_name)
            if invocation is None:
                # we may not have encountered an invocation if this function yet
                pass
            else:
                # TODO this currently only works for positional args
                assert len(node.args.args) == len(invocation)
                for i, arg_type_info in enumerate(invocation):
                    arg_node = node.args.args[i]
                    arg_name = arg_node.arg
                    arg_type_info = invocation[i]
                    self._register_type_info_by_node(arg_node, arg_type_info)
                    self._register_type_info_by_ident_name(arg_name, arg_type_info)
        elif num_children_visited == -1:
            if not func.has_explicit_return:
                func.register_rtn_type(context.TypeInfo.none())

    def rtn(self, node, num_children_visited):
        super().rtn(node, num_children_visited)
        if num_children_visited == -1:
            scope = self.ast_context.current_scope.get()
            func_name = scope.get_enclosing_namespace()
            assert func_name is not None, "return from what?"
            func = self.ast_context.get_function(func_name)
            func.has_explicit_return = True
            rtn_type_info = self.ast_context.lookup_type_info_by_node(node.value)
            self._assert_resolved_type(rtn_type_info, "cannot lookup rtn type info by node type %s" % node.value)
            if rtn_type_info is not None:
                func_name = scope.get_enclosing_namespace()
                assert func_name is not None, "return from what?"
                func = self.ast_context.get_function(func_name)
                assert func is not None
                func.register_rtn_type(rtn_type_info)

    def container_type_dict(self, node, num_children_visited):
        super().container_type_dict(node, num_children_visited)
        if num_children_visited == -1:
            type_info = self._register_literal_type(node, {})
            assert len(node.keys) == len(node.values)
            ctx = self.ast_context
            for i in range(0, len(node.keys)):
                key_node = node.keys[0]
                key_type_info = ctx.lookup_type_info_by_node(key_node)
                self._assert_resolved_type(key_type_info)
                type_info.register_contained_type(0, key_type_info)
                value_node = node.values[0]
                value_type_info = ctx.lookup_type_info_by_node(value_node)
                self._assert_resolved_type(value_type_info)
                type_info.register_contained_type(1, value_type_info)
            
    def container_type_list(self, node, num_children_visited):
        super().container_type_list(node, num_children_visited)
        if num_children_visited == -1:
            type_info = self._register_literal_type(node, [])
            for el in node.elts:
                ti = self.ast_context.lookup_type_info_by_node(el)
                self._assert_resolved_type(ti)
                type_info.register_contained_type(0, ti)

    def container_type_tuple(self, node, num_children_visited):
        super().container_type_tuple(node, num_children_visited)
        if num_children_visited == -1:
            # figure out whether all elements have the same type
            resolved_types = False
            homogeneous_types = False
            previous_type_info = None
            for el in node.elts:
                ti = self.ast_context.lookup_type_info_by_node(el)
                self._assert_resolved_type(ti)
                if ti is None:
                    resolved_types = False
                    break
                else:
                    resolved_types = True
                    if previous_type_info is not None:
                        if previous_type_info.value_type == ti.value_type:
                            homogeneous_types = True
                        else:
                            homogeneous_types = False
                            break
                    previous_type_info = ti

            if resolved_types:
                py_type = [] if homogeneous_types else ()
                type_info = self._register_literal_type(node, py_type)
                for i, el in enumerate(node.elts):
                    ti = self.ast_context.lookup_type_info_by_node(el)
                    self._assert_resolved_type(ti)
                    type_info.register_contained_type(i, ti)
                    if homogeneous_types:
                        break


    def compare(self, node, num_children_visited):
        super().compare(node, num_children_visited)
        if num_children_visited == -1:
            assert len(node.comparators) == 1
            self._register_literal_type(node, True) # register boolean type

    def loop_for(self, node, num_children_visited):
        super().loop_for(node, num_children_visited)
        if num_children_visited == 2: # type handling for loop iter/target
            # copied from assign and modified - probably some sharing is
            # desirable, once we add while?
            type_info = self.ast_context.lookup_type_info_by_node(node.iter)
            self._assert_resolved_type(type_info, "cannot lookup for loop target type by iter type %s" % node.iter)
            if type_info is not None:
                contained_type_info = type_info.get_contained_type_info()
                assert contained_type_info is not None, "don't know how to iterate over %s" % type_info
                # TODO copy type_info first?
                self._register_type_info_by_ident_name(self.lhs_loop_value,
                                                       contained_type_info)
                self._register_type_info_by_node(node.target, contained_type_info)

    def name(self, node, num_children_visited):
        super().name(node, num_children_visited)
        if self.visiting_attr:
            # target instance type determination:
            # for example n.startswith or n.append: associate the type of 'n'
            # with the name node 'n'
            type_info = self.ident_name_to_type_info.get(node.id, None)
            self._assert_resolved_type(type_info, "cannot lookup type info by id name %s" % node.id)
            if type_info is not None:
                self._register_type_info_by_node(node, type_info)
        elif self.visiting_func:
            func_name = node.id
            func = self.ast_context.get_function(func_name)
            func_rtn_type_info = func.get_rtn_type_info()
            self._register_type_info_by_node(node, func_rtn_type_info)
        elif self.assign_visiting_lhs:
            self.lhs_value = node.id
        elif self.loop_visiting_lhs:
            self.lhs_loop_value = node.id
        else:
            # a = b or printb() or any other b ref - lookup b's type
            type_info = self.ident_name_to_type_info.get(node.id, None)
            self._assert_resolved_type(type_info, "Cannot find type info for '%s'" % node.id)
            self._register_type_info_by_node(node, type_info)

    def num(self, node, num_children_visited):
        super().num(node, num_children_visited)
        self._register_literal_type(node, node.n)

    def string(self, node, num_children_visited):
        super().string(node, num_children_visited)
        self._register_literal_type(node, node.s)

    def constant(self, node, num_children_visited):
        super().constant(node, num_children_visited)        
        self._register_literal_type(node, node.value)

    def expr(self, node, num_children_visited):
        super().expr(node, num_children_visited)
        if num_children_visited == -1:
            type_info = self.ast_context.lookup_type_info_by_node(node.value)
            self._register_type_info_by_node(node, type_info)

    def _assert_resolved_type(self, type_thing, msg=""):
        # when all types have been determined, type_thing should not be None
        if type_thing is None:
            # uncomment to debug:
            #print("DEBUG %s" % msg)
            self.resolved_all_type_references = False

    def _register_type_info_by_ident_name(self, identifier_name, type_info):
        # None is allowed because there are multiple visits of the ast to
        # determine all types - initial calls to this method may pass in None
        assert type_info is None or isinstance(type_info, context.TypeInfo), "unexpected type %s" % type_info
        self.ident_name_to_type_info[identifier_name] = type_info

    def _register_type_info_by_node(self, node, type_info):
        # None is allowed because there are multiple visits of the ast to
        # determine all types - initial calls to this method may pass in None
        assert type_info is None or isinstance(type_info, context.TypeInfo), "unexpected type %s" % type_info
        self.ast_context.register_type_info_by_node(node, type_info)

    def _register_literal_type(self, node, value):
        # the literal_node_to_type_info mapping is specifically needed for
        # container types - when this visitor is going over the ast multiple
        # times, we need to make sure to re-use the same TypeInfo instance
        if node in self.literal_node_to_type_info:
            type_info = self.literal_node_to_type_info[node]
        else:
            type_info = context.TypeInfo(type(value))
            self.literal_node_to_type_info[node] = type_info
        self._register_type_info_by_node(node, type_info)
        return type_info


class BlockScopePuller(_CommonStateVisitor):

    def __init__(self, ast_context, syntax):
        super().__init__(ast_context, syntax)

    def name(self, node, num_children_visited):
        super().name(node, num_children_visited)
        if num_children_visited == 0:
            if not self.visiting_func:
                scope = self.ast_context.current_scope.get()
                if not scope.is_declaration_node(node):
                    if not scope.has_been_declared(node.id):
                        n = nodebuilder.constant_assignment(node.id, None)
                        scope.ast_node.body.insert(0, n)
