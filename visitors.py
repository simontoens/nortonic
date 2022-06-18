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
        self.visiting_func = False
        self.visiting_attr = False
        self.assign_visiting_lhs = False
        self.assign_visiting_rhs = False
        self.loop_visiting_lhs = False
        self.loop_visiting_rhs = False

        # needs to be a stack for nested func names, for example:
        # print("foo".startswith("f"))
        self.func_name_stack = []

    # returns the current func name
    def call(self, node, num_children_visited):
        if num_children_visited == 0:
            assert self.visiting_func == False
            self.visiting_func = True
        elif num_children_visited == 1:
            assert self.visiting_func == True
            self.visiting_func = False
        elif num_children_visited == -1:
            return self.func_name_stack.pop()
        return None

    def assign(self, node, num_children_visited):
        super().assign(node, num_children_visited)
        if num_children_visited == 0:
            assert not self.assign_visiting_lhs
            assert not self.assign_visiting_rhs
            self.assign_visiting_lhs = True
        elif num_children_visited != -1:
            self.assign_visiting_lhs = False
            self.assign_visiting_rhs = True
        else: # num_children_visited == -1
            self.assign_visiting_rhs = False

    def attr(self, node, num_children_visited):
        assert self.visiting_func
        if num_children_visited == 0:
            assert not self.visiting_attr
            self.visiting_attr = True
        if num_children_visited == -1:
            assert self.visiting_attr
            self.func_name_stack.append(node.attr)
            self.visiting_attr = False

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
            if not self.visiting_attr:
                self.func_name_stack.append(node.id)


class _TargetTypeVisitor(_CommonStateVisitor):
    """
    For methods/attributes, determines the type of the identifier being
    dereferenced.

    This visitor should only be used as a super class once types have been
    resolved.
    """

    def __init__(self, ast_context, syntax):    
        super().__init__(ast_context, syntax)
        self.target_type_stack = []

        # current target type: l.append -> target_type is 'list'
        self.target_type = None

    # returns (func_name, target_type)
    def call(self, node, num_children_visited):
        func_name = super().call(node, num_children_visited)
        if num_children_visited == -1:
            target_type = self.target_type_stack.pop()
            return func_name, target_type
        return None, None

    def constant(self, node, num_children_visited):
        super().constant(node, num_children_visited)
        if self.visiting_attr:
            self.target_type = type(node.value)

    def attr(self, node, num_children_visited):
        super().attr(node, num_children_visited)
        assert self.visiting_func
        if num_children_visited == 0:
            assert self.target_type is None
        if num_children_visited == -1:
            self.target_type_stack.append(self.target_type)
            self.target_type = None

    def name(self, node, num_children_visited):
        super().name(node, num_children_visited)
        if self.visiting_func:
            if self.visiting_attr:
                ti = self.ast_context.lookup_type_info_by_node(node)
                assert ti is not None
                # seems like this won't work well with nesting, we need another
                # stack?
                self.target_type = ti.value_type
            else:
                self.target_type_stack.append(None)


class FuncCallVisitor(_TargetTypeVisitor):
    """
    Executes rewrite rules on the AST - this visitor modifies the AST.
    """

    def __init__(self, ast_context, syntax):
        super().__init__(ast_context, syntax)
        self._rewritten_nodes = []

        # True until all work is done and this visitor doesn't need to run
        # anymore
        self._revisit = True

    @property
    def keep_visiting(self):
        return self._revisit

    def done(self):
        self._revisit = len(self._rewritten_nodes) > 0
        self._rewritten_nodes = []

    def assign(self, node, num_children_visited):
        super().assign(node, num_children_visited)
        if num_children_visited == -1:
            assert len(node.targets) == 1
            # use '=' to transform into a function call
            self._handle_function_call("<>_=", None, node, arg_nodes=[node.targets[0], node.value])

    def binop(self, node, num_children_visited):
        super().binop(node, num_children_visited)
        if num_children_visited == -1:
            if isinstance(node.op, ast.Add):
                op = "+"
            elif isinstance(node.op, ast.Mult):
                op = "*"
            else:
                assert False, "Unhandled binop"
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
        func_name, target_type = super().call(node, num_children_visited)
        if num_children_visited == -1:
            assert func_name is not None
            self._handle_function_call(func_name, target_type, node, node.args)

    def cond_if(self, node, num_children_visited):
        super().cond_if(node, num_children_visited)
        if num_children_visited == -1:
            # we'll pretend this is a function call so we have a rewrite hook
            self._handle_function_call("<>_if", None, node, arg_nodes=[node.test])

    def loop_for(self, node, num_children_visited):
        super().loop_for(node, num_children_visited)
        if num_children_visited == -1:
            self._handle_function_call("<>_loop_for", None, node, arg_nodes=[node.target, node.iter])

    def subscript(self, node, num_children_visited):
        super().subscript(node, num_children_visited)
        if num_children_visited == -1:
            # TODO don't hardcode target_type!!
            self._handle_function_call("<>_[]", list, node, arg_nodes=[node.value, node.slice])

    def funcdef(self, node, num_children_visited):
        super().funcdef(node, num_children_visited)
        if num_children_visited == -1:
            self._handle_function_call("<>_funcdef", None, node, arg_nodes=node.args.args)

    def _handle_function_call(self, func_name, target_type, node, arg_nodes):
        if hasattr(node, nodeattrs.REWRITTEN_NODE_ATTR):
            return
        if func_name in self.syntax.functions:
            func = self.syntax.functions[func_name]
            # make sure the target types match so that if rewriting
            # list.append is requested we don't rewrite custom_type.append
            if target_type == func.py_type:
                args = []
                for arg_node in arg_nodes:
                    type_info = self.ast_context.lookup_type_info_by_node(arg_node)
                    assert type_info is not None, "unable to lookup type info for function %s: arg %s" % (func_name, arg_node)
                    args.append(syntax.Argument(arg_node, type_info.value_type))
                rw = astrewriter.ASTRewriter(node, arg_nodes, self.ast_context)
                # the actual AST rewriting happens here:
                if func.target_name is not None:
                    rw.rename(func.target_name)
                if func.function_rewrite is not None:
                    func.function_rewrite(args, rw)
                self._rewritten_nodes.append(node)
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

        # the TypeInfo instance of the node being dereferenced,
        # when visiting an attr - n.append -> list
        self.target_instance_type_info = None

        # True until all work is done and this visitor doesn't need to run
        # anymore
        self.revisit = True

        self.num_visits = 0

    @property
    def keep_visiting(self):
        return self.revisit

    def done(self):
        if self.resolved_all_type_references:
            self.revisit = False
        else:
            assert self.num_visits < 6, "cannot resolve all references, check method _assert_resolved_type"
                
            self.revisit = True
            self.resolved_all_type_references = True
            self.num_visits += 1

    def assign(self, node, num_children_visited):
        super().assign(node, num_children_visited)
        if num_children_visited == -1:
            # add mapping of lhs id name -> to its type
            type_info = self.ast_context.lookup_type_info_by_node(node.value)
            self._assert_resolved_type(type_info, "Unable to lookup type of assignment rhs %s" % node.value)
            # TODO copy type_info first?
            self._register_type_info_by_ident_name(self.lhs_value, type_info)
            assert len(node.targets) == 1
            self._register_type_info_by_node(node.targets[0], type_info)

    def subscript(self, node, num_children_visited):
        super().subscript(node, num_children_visited)
        if num_children_visited == -1:
            # similar to for loop
            type_info = self.ast_context.lookup_type_info_by_node(node.value)
            self._assert_resolved_type(type_info, "cannot lookup for loop target type by iter type %s" % node.value)
            if type_info is not None:
                contained_type = type_info.get_homogeneous_contained_type(0)
                contained_type_info = context.TypeInfo(contained_type)
                self._register_type_info_by_node(node, contained_type_info)

    def attr(self, node, num_children_visited):
        super().attr(node, num_children_visited)
        if num_children_visited == -1:
            func_name = node.attr
            assert self.target_instance_type_info is not None
            method = self.ast_context.get_method(
                func_name, self.target_instance_type_info.value_type)
            assert method is not None, "Unknown attr %s" % func_name
            self._register_type_info_by_node(node, method.get_rtn_type_info())

    def binop(self, node, num_children_visited):
        super().binop(node, num_children_visited)
        if num_children_visited == -1:
            self._register_node_target_type(node, node.left, node.right)

    def call(self, node, num_children_visited):
        func_name = super().call(node, num_children_visited)
        if num_children_visited == -1:
            # record this function invocation so we know the argument types
            # it is called with
            # when this visitor runs multiple times, this keeps re-adding the
            # same invocation - dedupe?
            arg_type_infos = [self.ast_context.lookup_type_info_by_node(a) for a in node.args]
            # for methods, we should lookup the right method, based on
            # target_instance_type - since we have not implemented user
            # defined methods, it doesn't matter right now
            self.ast_context.get_function(func_name).register_invocation(arg_type_infos)
            # TODO
            # - need this metadata:
            # TypeInfo.is_container_type
            # Method.populates_container_type
            if self.target_instance_type_info is not None:
                if len(arg_type_infos) > 0:
                    self.target_instance_type_info.register_contained_type_1(arg_type_infos[0].value_type)
            
            # propagate the return type from the func child node to this call
            # parent node
            rtn_type_info = self.ast_context.lookup_type_info_by_node(node.func)
            assert rtn_type_info is not None, "no rtn type for %s" % node.func
            self.ast_context.register_type_info_by_node(node, rtn_type_info)

    def funcdef(self, node, num_children_visited):
        super().funcdef(node, num_children_visited)
        if num_children_visited == 0:
            func_name = node.name
            func = self.ast_context.get_function(func_name)
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

    def rtn(self, node, num_children_visited):
        super().rtn(node, num_children_visited)
        if num_children_visited == -1:
            rtn_type_info = self.ast_context.lookup_type_info_by_node(node.value)
            self._assert_resolved_type(rtn_type_info, "cannot lookup rtn type info by node type %s" % node.value)
            scope = self.ast_context.current_scope.get()
            func_name = scope.get_enclosing_namespace()
            assert func_name is not None, "return from what?"
            func = self.ast_context.get_function(func_name)
            assert func is not None
            func.register_rtn_type(rtn_type_info)

    def container_type_dict(self, node, num_children_visited):
        super().container_type_list(node, num_children_visited)
        if num_children_visited == -1:
            type_info = self._register_literal_type(node, {})
            assert len(node.keys) == len(node.values)
            ctx = self.ast_context
            for i in range(0, len(node.keys)):
                key_node = node.keys[0]
                key_type = ctx.lookup_type_info_by_node(key_node).value_type
                self._assert_resolved_type(key_type)
                type_info.register_contained_type_1(key_type)
                value_node = node.values[0]
                value_type = ctx.lookup_type_info_by_node(value_node).value_type
                self._assert_resolved_type(value_type)
                type_info.register_contained_type_2(value_type)
            
    def container_type_list(self, node, num_children_visited):
        super().container_type_list(node, num_children_visited)
        if num_children_visited == -1:
            type_info = self._register_literal_type(node, [])
            for el in node.elts:
                t = self.ast_context.lookup_type_info_by_node(el).value_type
                self._assert_resolved_type(t)
                type_info.register_contained_type_1(t)

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
                contained_type = type_info.get_homogeneous_contained_type(0)
                assert contained_type is not None, "don't know how to iterate over %s" % type_info
                # TODO copy type_info first?
                contained_type_info = context.TypeInfo(contained_type)
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
                self.target_instance_type_info = type_info
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
            # a = b or print(b) or any other b ref - lookup b's type
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
        if self.visiting_attr:
            # target instance type determination:
            # for example "foo".startswith: associate the type of 'n' with the
            # name node 'n'
            self.target_instance_type_info = context.TypeInfo(type(node.value))

    def expr(self, node, num_children_visited):
        super().expr(node, num_children_visited)
        if num_children_visited == -1:
            type_info = self.ast_context.lookup_type_info_by_node(node.value)
            self._register_type_info_by_node(node, type_info)

    def _assert_resolved_type(self, type_thing, msg=""):
        # when all types have been determined, type_thing should not be None
        if type_thing is None:
            # uncomment to debug
            # print("DEBUG %s" % msg)
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
        type_info = context.TypeInfo(type(value))
        self._register_type_info_by_node(node, type_info)
        return type_info

    def _register_node_target_type(self, target_node, lhs_node, rhs_node):
        lhs_type_info = self.ast_context.lookup_type_info_by_node(lhs_node)
        assert lhs_type_info is not None, "Unable to lookup LHS node type"
        rhs_type_info = self.ast_context.lookup_type_info_by_node(rhs_node)
        assert rhs_type_info is not None, "Unable to find RHS node type"
        target_type = self.syntax.combine_types(lhs_type_info.value_type,
                                                rhs_type_info.value_type)
        target_type_info = context.TypeInfo(target_type)
        self._register_type_info_by_node(target_node, target_type_info)


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
