from target import targetlanguage
import ast
import astpath
import astrewriter
import context
import copy
import nodeattrs
import nodebuilder
import types
import visitor


class _CommonStateVisitor(visitor.NoopNodeVisitor):
    """
    Boolean state based on nodes being visited.
    """

    def __init__(self, ast_context, target):
        super().__init__()
        self.ast_context = ast_context
        self.target = target
        self.assign_visiting_lhs = False
        self.assign_visiting_rhs = False
        self.loop_visiting_lhs = False
        self.loop_visiting_rhs = False
        self.visiting_rtn = False

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
        elif num_children_visited == 1:
            self.parent_node_stack.pop()
        elif num_children_visited == -1:
            name =  self.func_name_stack.pop()
            return name
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
            self.parent_node_stack.append(node)
        if num_children_visited == -1:
            self.parent_node_stack.pop()
            if self.visiting_func:
                self.func_name_stack.append(node.attr)

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
            self.func_name_stack.append(node.id)

    def rtn(self, node, num_children_visited):
        super().rtn(node, num_children_visited)
        if num_children_visited == 0:
            assert not self.visiting_rtn
            self.visiting_rtn = True
        elif num_children_visited == -1:
            assert self.visiting_rtn
            self.visiting_rtn = False

    def _reset(self):
        self.assign_visiting_lhs = False
        self.assign_visiting_rhs = False
        self.loop_visiting_lhs = False
        self.loop_visiting_rhs = False
        self.func_name_stack = []
        self.parent_node_stack = []


class _BodyParentNodeVisitor(visitor.NoopNodeVisitor):

    def __init__(self):
        super().__init__()
        self.body_parent_node_stack = []

    @property
    def body_parent_node(self):
        return self.body_parent_node_stack[-1]

    def block(self, node, num_children_visited, is_root_block):
        if num_children_visited == 0:
            self.body_parent_node_stack.append(node)
        elif num_children_visited == -1:
            self.body_parent_node_stack.pop()


class FuncCallVisitor(_CommonStateVisitor, _BodyParentNodeVisitor):
    """
    Executes rewrite rules on the AST - this visitor modifies the AST.
    """
    def __init__(self, ast_context, target):
        super().__init__(ast_context, target)
        self._keep_revisiting = False

    @property
    def leave_early(self):
        # do we actually need this - tests pass without this (?)
        return self._keep_revisiting

    @property
    def should_revisit(self):
        if self._keep_revisiting:
            self._keep_revisiting = False
            super()._reset()
            return True
        return False

    def assign(self, node, num_children_visited):
        if not hasattr(node, nodeattrs.REWRITTEN_NODE_ATTR):
            super().assign(node, num_children_visited)
            if num_children_visited == -1:
                assert len(node.targets) == 1
                lhs = node.targets[0]
                if isinstance(lhs, ast.Subscript):
                    # Python "add to dict" syntax: d[key] = value - provide a
                    # rewrite hook at the assigment node level
                    # (similar checks exist in other visitors)
                    # lhs.value: dict instance
                    # lhs.slice: key
                    # node.value: value
                    self._handle_function_call("<>_dict_assignment", lhs.value, node, arg_nodes=[lhs.slice, node.value])
                else:
                    # use '=' to transform into a function call
                    self._handle_function_call("<>_=", None, node, arg_nodes=[lhs.get(), node.value.get()])

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
            elif isinstance(node.op, ast.Mod):
                op = "%"
            else:
                assert False, "Unhandled binop %s" % node.op
            self._handle_function_call("<>_%s" % op, None, node, [node.left, node.right])

    def boolop(self, node, num_children_visited):
        super().boolop(node, num_children_visited)
        if num_children_visited == -1:
            if isinstance(node.op, ast.And):
                op = "&&"
            elif isinstance(node.op, ast.Or):
                op = "||"
            else:
                assert False, "Unhandled boolop %s" % node.op
            self._handle_function_call("<>_%s" % op, None, node, node.values)

    def compare(self, node, num_children_visited):
        super().compare(node, num_children_visited)
        if num_children_visited == -1:
            assert len(node.ops) == 1
            assert len(node.comparators) == 1
            if isinstance(node.ops[0], ast.Eq):
                op = "<>_=="
            elif isinstance(node.ops[0], ast.Is):
                op = "<>_is"
            else:
                assert False, "Unhandled comparison %s" % node.ops[0]
            self._handle_function_call(op, None, node, [node.left, node.comparators[0]])

    def attr(self, node, num_children_visited):
        super().attr(node, num_children_visited)
        if num_children_visited == -1:
            attr_name = node.attr
            target_node = node.value
            args = []
            self._handle_function_call(attr_name, target_node, node, args)

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
        arg_nodes = [a.get() for a in arg_nodes]
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
        rewrite_target = self._lookup_rewrite_target(func_name, target_type, node)
        if rewrite_target is not None:
            args = []
            for arg_node in arg_nodes:
                type_info = self.ast_context.lookup_type_info_by_node(arg_node)
                assert type_info is not None, "unable to lookup type info for function %s: arg %s" % (func_name, arg_node)
                args.append(targetlanguage.Argument(arg_node, type_info.value_type))
            rw = astrewriter.ASTRewriter(node,
                                         arg_nodes,
                                         self.ast_context,
                                         self.body_parent_node,
                                         target_node)

            # the actual AST rewriting happens here:
            if rewrite_target.target_name is not None:
                rw.rename(rewrite_target.target_name)
            if rewrite_target.function_rewrite is not None:
                rewrite_target.function_rewrite(args, rw)
            self._keep_revisiting = True
            setattr(node, nodeattrs.REWRITTEN_NODE_ATTR, True)

    def _lookup_rewrite_target(self, func_name, target_type, node):
        # currently we distinguish between function/method == "Call"
        # rewrites and attribute rewrites, for example:
        # os.path.join() <- call
        # os.path.sep <- attr
        attr_path = None
        if target_type is types.ModuleType:
            attr_path = astpath.get_attr_path(node)
        key = self.target.get_function_lookup_key(func_name, target_type, attr_path, type(node))
        if key not in self.target.functions:
            key = self.target.get_function_lookup_key(func_name, target_type=None, ast_path=attr_path, target_node_type=type(node))
        if key in self.target.functions:
             return self.target.functions[key]
        return None


class TypeVisitor(_CommonStateVisitor):
    """
    This visitor determines the type of every AST Node.
    """

    def __init__(self, ast_context, targe):
        super().__init__(ast_context, targe)

        # starts out True, set to False when an unresolved type is encountered
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
            lhs = node.targets[0].get()
            rhs = node.value.get()
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
            elif isinstance(lhs, ast.Tuple):
                if rhs_type_info is not None:
                    for i, unpacked_lhs in enumerate(lhs.elts):
                        ti = rhs_type_info.get_contained_type_info_at(i)
                        self._assert_resolved_type(ti, "Unable to lookup contained type of assigned rhs (unpacking) %s" % rhs_type_info)
                        if ti is not None:
                            self._register_type_info_by_node(unpacked_lhs, ti)
            else:
                # associate the type of the RHS with the LHS node
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
                    # container (list, dict ...)
                    if type_info.is_sequence and isinstance(node.slice, ast.Constant):
                        # if index is by constant number, ie a = l[2], look up
                        # specific type at that slot
                        contained_type_info = type_info.get_contained_type_info_at(node.slice.value)
                    else:
                        contained_type_info = type_info.get_contained_type_info(is_subscript=True)
                    self._assert_resolved_type(contained_type_info, "cannot lookup contained type of subscript expression %s" % node.value)                
                    self._register_type_info_by_node(node, contained_type_info)

    def attr(self, node, num_children_visited):
        super().attr(node, num_children_visited)
        if num_children_visited == -1:
            # foo.blah() -> the type of foo
            target_instance_type_info = self.ast_context.lookup_type_info_by_node(node.value)
            self._assert_resolved_type(target_instance_type_info, "cannot determine type of target instance %s" % node.value)
            if target_instance_type_info is not None:
                func_name = node.attr
                method = self.ast_context.get_method(func_name, target_instance_type_info)
                assert method is not None, "Unknown attr [%s]" % func_name
                self._register_type_info_by_node(node, method.get_rtn_type_info())

    def unaryop(self, node, num_children_visited):
        super().binop(node, num_children_visited)
        if num_children_visited == -1:
            type_info = self.ast_context.lookup_type_info_by_node(node.operand)
            self._assert_resolved_type(type_info, "unaryop: missing type information for operand %s" % node.operand)
            self._register_type_info_by_node(node, type_info)

    def boolop(self, node, num_children_visited):
        super().boolop(node, num_children_visited)
        if num_children_visited == -1:
            self._register_type_info_by_node(node, context.TypeInfo.bool())

    def binop(self, node, num_children_visited):
        super().binop(node, num_children_visited)
        if num_children_visited == -1:
            lhs = node.left
            lhs_type_info = self.ast_context.lookup_type_info_by_node(lhs)
            self._assert_resolved_type(lhs_type_info, "binop: missing type information for lhs %s" % lhs)
            rhs = node.right
            rhs_type_info = self.ast_context.lookup_type_info_by_node(rhs)
            self._assert_resolved_type(rhs_type_info, "binop: missing type information for rhs %s" % rhs)
            if lhs_type_info is not None and rhs_type_info is not None:
                coercion_rule = self.target.type_mapper.lookup_type_coercion_rule(lhs_type_info, rhs_type_info)
                if coercion_rule is not None:
                    target_type = coercion_rule.result_type
                    if coercion_rule.rhs_conversion_function_name is not None:
                        arg_node = copy.copy(rhs)
                        conv_node = nodebuilder.call(
                            coercion_rule.rhs_conversion_function_name,
                            (copy.copy(rhs),))
                        setattr(rhs, nodeattrs.ALT_NODE_ATTR, conv_node)
                        target_type_info = context.TypeInfo(target_type)
                        self._register_type_info_by_node(conv_node, target_type_info)
                else:
                    target_type = self.target.combine_types(
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
                target_instance_type_info = self.ast_context.lookup_type_info_by_node(node.func.value) if isinstance(node.func, ast.Attribute) else None
                if target_instance_type_info is None:
                    func = self.ast_context.get_function(func_name)
                else:
                    func = self.ast_context.get_method(func_name, target_instance_type_info)
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

                # propagate the return type of the func to this call node
                rtn_type_info = func.get_rtn_type_info()
                self._assert_resolved_type(rtn_type_info, "no rtn type for func %s %s" % (func.name, node.func))
                if rtn_type_info is not None:
                    if rtn_type_info.has_late_resolver:
                        rtn_type_info = rtn_type_info.apply_late_resolver(arg_type_infos[0])

                    self.ast_context.register_type_info_by_node(node, rtn_type_info)

    def cond_if(self, node, num_children_visited, is_expr):
        super().cond_if(node, num_children_visited, is_expr)
        if is_expr:
            if num_children_visited == -1:
                # check both the if and the else branch for type info, otherwise
                # expressions like this won't work:
                # a = 2 if 1 > 0 else None
                if_ti = self.ast_context.lookup_type_info_by_node(node.body[0])
                else_ti = self.ast_context.lookup_type_info_by_node(node.orelse[0])
                if self._assert_resolved_type([if_ti, else_ti], "cannot figure out rtn type for if expr %s" % node):
                    ti = context.TypeInfo.find_significant([if_ti, else_ti])
                    self._register_type_info_by_node(node, ti)

    def funcdef(self, node, num_children_visited):
        super().funcdef(node, num_children_visited)
        func_name = node.name
        func = self.ast_context.get_function(func_name)
        if num_children_visited == 0:
            if len(node.args.args) > 0:
                # lookup invocations to determine the argument types
                invocation = func.invocations[0] if len(func.invocations) > 0 else None
                self._assert_resolved_type(invocation, "cannot find invocation of function %s" % func_name)
                if invocation is not None:
                    # TODO this currently only works for positional args
                    assert len(node.args.args) == len(invocation)
                    for i, arg_type_info in enumerate(invocation):
                        arg_node = node.args.args[i]
                        arg_name = arg_node.arg
                        arg_type_info = invocation[i]
                        self._register_type_info_by_node(arg_node, arg_type_info)
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
                py_type = ()                
                if self.target.function_can_return_multiple_values and self.visiting_rtn:
                    # if the language supports multiple rtn values, we need to
                    # track the type of each of them, even if they are the same
                    # types - for example, if we have
                    # return 1, 1, we need to know int, int
                    #
                    # this is probably not covering all use cases, ...
                    homogeneous_types = False
                elif homogeneous_types:
                    # this makes life easier for Java (others?) - if we have
                    # l = [1, 2, 3], we use
                    # List<Integer>, not Tuple<Integer, Integer>
                    # (... ignoring the fact that Tuple doesn't exist)
                    # py_type = []
                    pass
                type_info = self._register_literal_type(node, py_type)
                for i, el in enumerate(node.elts):
                    ti = self.ast_context.lookup_type_info_by_node(el)
                    self._assert_resolved_type(ti)
                    type_info.register_contained_type(i, ti)
                    if homogeneous_types:
                        # since all contained types are the same, we can stop
                        break
                

    def compare(self, node, num_children_visited):
        super().compare(node, num_children_visited)
        if num_children_visited == -1:
            assert len(node.comparators) == 1
            self._register_literal_type(node, True) # register boolean type

    def loop_for(self, node, num_children_visited):
        super().loop_for(node, num_children_visited)
        if num_children_visited == 2: # type handling for loop iter/target
            # this method is similar to assign, refactor to share
            type_info = self.ast_context.lookup_type_info_by_node(node.iter)
            self._assert_resolved_type(type_info, "cannot lookup for loop target type by iter type %s" % node.iter)
            if type_info is not None:
                contained_type_info = type_info.get_contained_type_info()
                assert contained_type_info is not None, "don't know how to iterate over %s" % type_info
                if contained_type_info is not None:
                    target = node.target.get()
                    if isinstance(target, ast.Tuple):
                        for i, unpacked_lhs in enumerate(target.elts):
                            ti = contained_type_info.get_contained_type_info_at(i)
                            self._assert_resolved_type(ti, "Unable to lookup contained type of loop target (unpacking) %s" % contained_type_info)
                            if ti is not None:
                                self._register_type_info_by_node(unpacked_lhs, ti)
                    else:
                        self._register_type_info_by_node(target, contained_type_info)

    def name(self, node, num_children_visited):
        super().name(node, num_children_visited)
        if self.visiting_func:
            func_name = node.id
            func = self.ast_context.get_function(func_name)
            func_rtn_type_info = func.get_rtn_type_info()
            self._register_type_info_by_node(node, func_rtn_type_info)
        elif self.assign_visiting_lhs:
            pass
        elif self.loop_visiting_lhs:
            pass
        else:
            # a = b or print(b) or any other b ref - lookup b's type
            if self.visiting_attr:
                # target instance type determination:
                # for example n.startswith or n.append: associate the type of
                # 'n' with the name node 'n'
                pass
            type_info = self._lookup_type_info_by_ident_name(node.id)
            self._assert_resolved_type(type_info, "cannot find type info for ident '%s'" % node.id)
            if type_info is not None:
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

    def import_stmt(self, node, num_children_visited):
        if num_children_visited == 0:
            super().import_stmt(node, num_children_visited)
            for alias_node in node.names:
                self._register_type_info_by_node(alias_node, context.TypeInfo.module(alias_node.name))

    def on_scope_released(self, scope):
        super().on_scope_released(scope)
        for ident_name in scope.get_identifiers_in_this_scope():
            for ident_node in scope.get_identifier_nodes_in_this_scope(ident_name):
                type_info = self.ast_context.lookup_type_info_by_node(ident_node)
                self._assert_resolved_type(type_info, "on_scope_release cannot lookup type of %s" % ident_name)
                if type_info is not None:
                    declaration_node = scope.get_declaration_node(ident_name)
                    assert declaration_node is not None
                    decl_type_info = self.ast_context.lookup_type_info_by_node(declaration_node)
                    if decl_type_info is None or decl_type_info.is_none_type:
                        self._register_type_info_by_node(declaration_node, type_info)
                    elif type_info.value_type != decl_type_info.value_type:
                        assert not self.target.strongly_typed, "ident [%s] cannot be both a %s and a %s" % (ident_name, type_info, decl_type_info)

    def _lookup_type_info_by_ident_name(self, ident_name):
        scope = self.ast_context.current_scope.get()
        declaration_node = scope.get_declaration_node(ident_name)
        assert declaration_node is not None
        return self.ast_context.lookup_type_info_by_node(declaration_node)

    def _assert_resolved_type(self, type_thing, msg=""):
        if not isinstance(type_thing, (list, tuple)):
            # multiple types can optionally be passed in
            type_thing = [type_thing]
        # when all types have been determined, type_thing should not be None
        for t in type_thing:
            if t is None:
                # uncomment to debug:
                # print("DEBUG %s" % msg)
                self.resolved_all_type_references = False
                break
        else:
            return True
        return False

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

    def __init__(self, ast_context, target):
        super().__init__(ast_context, target)

    def name(self, node, num_children_visited):
        super().name(node, num_children_visited)
        if num_children_visited == 0:
            if not self.visiting_func:
                scope = self.ast_context.current_scope.get()
                if not scope.is_declaration_node(node):
                    if not scope.has_been_declared(node.id):
                        n = nodebuilder.constant_assignment(node.id, None)
                        scope.ast_node.body.insert(0, n)


class WithRemover(visitor.NoopNodeVisitor):
    """
    Removes With/Try/Except until we support those.
    """

    def __init__(self, ast_context):
        super().__init__()
        self.ast_context = ast_context

    def with_resource(self, node, num_children_visited):
        super().with_resource(node, num_children_visited)
        if num_children_visited == 0:
            scope = self.ast_context.current_scope.get()
            insert_index = scope.body_index(node) + 1
            for i, item in enumerate(node.items):
                n = nodebuilder.assignment(item.optional_vars, item.context_expr)
                scope.ast_node.body.insert(insert_index + i, n)
            for i, b in enumerate(node.body):
                scope.ast_node.body.insert(insert_index + i + len(node.items), b)


class DocStringHandler(visitor.NoopNodeVisitor):
    """
    Finds doc strings, removes them from the ast and associates them with
    their method definition.
    """
    def __init__(self, ast_context):
        super().__init__()
        self.ast_context = ast_context

    def funcdef(self, node, num_children_visited):
        super().funcdef(node, num_children_visited)
        if num_children_visited == -1:
            if isinstance(node.body[0], ast.Expr):
                if isinstance(node.body[0].value, ast.Constant):
                    if isinstance(node.body[0].value.value, str):
                        func_name = node.name
                        func = self.ast_context.get_function(func_name)
                        func.docstring = node.body[0].value.value
                        del node.body[0]


class UnpackingRewriter(_BodyParentNodeVisitor):
    """
    a, b = [1, 2]

    =>

    t0 = [1, 2]
    a = t0[0]
    b = t0[1]
    """
    def __init__(self, ast_context,
                 has_assignment_lhs_unpacking,
                 function_can_return_multiple_values):
        super().__init__()
        self.ast_context = ast_context
        self.has_assignment_lhs_unpacking = has_assignment_lhs_unpacking
        self.function_can_return_multiple_values = function_can_return_multiple_values

    def assign(self, node, num_children_visited):
        super().assign(node, num_children_visited)
        if num_children_visited == 0:
            assert len(node.targets) == 1
            lhs = node.targets[0].get()
            rhs = node.value.get()
            rewrite = self._should_rewrite(lhs, rhs)
            if rewrite:
                if isinstance(rhs, ast.Name):
                    ident_node = rhs
                    # skip the original assignment node
                    setattr(node, nodeattrs.SKIP_NODE_ATTR, True)
                else:
                    ident_name = self.ast_context.get_unqiue_identifier_name()
                    ident_node = nodebuilder.identifier(ident_name)
                    setattr(lhs, nodeattrs.ALT_NODE_ATTR, ident_node)
                self._add_subscribt_assignments(node, ident_node, lhs.elts)

    def loop_for(self, node, num_children_visited):                    
        super().loop_for(node, num_children_visited)
        if num_children_visited == 0:
            rewrite = self._should_rewrite(node.target, node.iter)
            if rewrite:
                ident_name = self.ast_context.get_unqiue_identifier_name()
                ident_node = nodebuilder.identifier(ident_name)
                setattr(node.target, nodeattrs.ALT_NODE_ATTR, ident_node)
                for i, target_node in enumerate(node.target.elts):
                    n = nodebuilder.assignment(
                        target_node,
                        nodebuilder.subscript_list(ident_name, i))
                    node.body.insert(i, n)

    def _add_subscribt_assignments(self, node, list_ident_node, target_nodes):
        insert_index = nodebuilder.get_body_insert_index(self.body_parent_node, node) + 1
        varname = list_ident_node.id
        body_node = self.body_parent_node
        for i in range(len(target_nodes)):
            n = nodebuilder.assignment(
                target_nodes[i],
                nodebuilder.subscript_list(varname, i))
            body_node.body.insert(insert_index + i, n)

    def _should_rewrite(self, lhs, rhs):
        rewrite = isinstance(lhs, ast.Tuple)
        if isinstance(rhs, ast.Call):
            if self.function_can_return_multiple_values:
                rewrite = False
        else:
            if self.has_assignment_lhs_unpacking:
                rewrite = False
        return rewrite


class IdentifierCollector(visitor.NoopNodeVisitor):
    """
    Collects all identifier names, so that when new identifier names are
    generated, we can avoid clashes.
    """
    def __init__(self, ast_context):
        super().__init__()
        self.ast_context = ast_context
        self.ident_names = set()

    @property
    def should_revisit(self):
        self.ast_context.register_ident_names(self.ident_names)
        return False

    def on_scope_released(self, scope):
        super().on_scope_released(scope)
        self.ident_names.update(scope.get_identifiers_in_this_scope())
