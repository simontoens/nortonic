import ast

from visitor import visitors
import copy
import context
import nodeattrs
import nodebuilder


class TypeVisitor(visitors._CommonStateVisitor):
    """
    This visitor determines the type of every AST Node.

    It also create Function instances and attaches them to Funcdef and Call
    nodes.
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

    def visited(self):
        super().visited()
        for f in self.ast_context.get_user_functions():
            f.reduce_type_infos()

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
                        self._register_type_info_by_node(unpacked_lhs, ti)
            else:
                self._register_type_info_by_node(lhs, rhs_type_info)
                self._register_type_info_by_node(node, rhs_type_info)

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
                        # FIXME assumes dict - need to check target type
                        contained_type_info = type_info.get_contained_type_info_at(1)
                    self._assert_resolved_type(contained_type_info, "cannot lookup contained type of subscript expression %s" % node.value)
                    self._register_type_info_by_node(node, contained_type_info)

    def assign_aug(self, node, num_children_visited):
        super().assign_aug(node, num_children_visited)
        if num_children_visited == -1:
            type_info = self.ast_context.lookup_type_info_by_node(node.value)
            self._assert_resolved_type(type_info, "cannot lookup type of assign_aug node.value %s" % node.value)
            self._register_type_info_by_node(node, type_info)
                    
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
            lhs = node.left.get()
            lhs_type_info = self.ast_context.lookup_type_info_by_node(lhs)
            self._assert_resolved_type(lhs_type_info, "binop: missing type information for lhs %s" % lhs)
            rhs = node.right.get()
            rhs_type_info = self.ast_context.lookup_type_info_by_node(rhs)
            self._assert_resolved_type(rhs_type_info, "binop: missing type information for rhs %s" % rhs)
            if lhs_type_info is not None and rhs_type_info is not None:
                coercion_rule = self.target.type_mapper.lookup_type_coercion_rule(lhs_type_info, rhs_type_info)
                if coercion_rule is not None:
                    target_type = coercion_rule.result_type
                    coercion_fname = coercion_rule.rhs_conversion_function_name
                    if coercion_fname is not None:
                        arg_node = copy.copy(rhs)
                        conv_node = nodebuilder.call(coercion_fname, [arg_node])
                        setattr(rhs, nodeattrs.ALT_NODE_ATTR, conv_node)
                        target_type_info = context.TypeInfo(target_type)
                        self._register_type_info_by_node(conv_node, target_type_info)
                        # register a function for this coercion rule so that
                        # when this visitor runs for the 2nd time, we can go
                        # through the regular code path that expects a function
                        # instance with a rtn type to exist (see call())
                        self.ast_context.get_function(coercion_fname)\
                            .register_rtn_type(target_type_info)
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
                nodeattrs.set_function(node, func)
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
                    if self._assert_resolved_type(target_instance_type_info, "Cannot lookup type info of target %s" % node.func.value):
                        assert len(arg_type_infos) > 0
                        target_instance_type_info.register_contained_type(0, arg_type_infos[0])

                # propagate the return type of the func to this call node
                rtn_type_info = func.get_rtn_type_info()
                self._assert_resolved_type(rtn_type_info, "no rtn type for func %s %s" % (func.name, node.func))
                if rtn_type_info is not None:
                    if rtn_type_info.has_late_resolver:
                        rtn_type_info = rtn_type_info.apply_late_resolver(arg_type_infos[0])

                    self.ast_context.register_type_info_by_node(node, rtn_type_info)

    def cond_if(self, node, num_children_visited):
        super().cond_if(node, num_children_visited)
        self._register_type_info_by_node(node, context.TypeInfo.notype())

    def cond_if_expr(self, node, num_children_visited):
        super().cond_if_expr(node, num_children_visited)
        if num_children_visited == -1:
            # check both the if and the else branch for type info, otherwise
            # expressions like this won't work:
            # a = 2 if 1 > 0 else None
            if_ti = self.ast_context.lookup_type_info_by_node(node.body)
            else_ti = self.ast_context.lookup_type_info_by_node(node.orelse)
            if self._assert_resolved_type([if_ti, else_ti], "cannot figure out rtn type for if expr %s" % node):
                ti = context.TypeInfo.get_homogeneous_type([if_ti, else_ti], allow_none_matches=True)
                self._register_type_info_by_node(node, ti)

    def funcdef(self, node, num_children_visited):
        super().funcdef(node, num_children_visited)
        func_name = node.name
        func = self.ast_context.get_function(func_name)
        nodeattrs.set_function(node, func)
        if num_children_visited == 0:
            self._register_type_info_by_node(node, context.TypeInfo.notype())
            if len(node.args.args) > 0:
                # lookup previous invocation to determine the argument types
                invocation = func.invocation
                self._assert_resolved_type(invocation, "cannot find invocation of function %s" % func_name)
                if invocation is not None:
                    # TODO this currently only works for positional args
                    assert len(node.args.args) == len(invocation)
                    for i, arg_type_info in enumerate(invocation):
                        arg_node = node.args.args[i]
                        arg_name = arg_node.arg
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
            nodeattrs.set_function(node, func)
            func.has_explicit_return = True
            rtn_type_info = self.ast_context.lookup_type_info_by_node(node.value)
            self._assert_resolved_type(rtn_type_info, "cannot lookup rtn type info by node type %s" % node.value)
            if rtn_type_info is not None:
                func.register_rtn_type(rtn_type_info)
                func.returns_literal = not isinstance(node.value, ast.Name)

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
        self._handle_container_type(node, num_children_visited, [])

    def container_type_tuple(self, node, num_children_visited):
        super().container_type_tuple(node, num_children_visited)
        self._handle_container_type(node, num_children_visited, ())
        
    def _handle_container_type(self, node, num_children_visited, py_container):
        if num_children_visited == -1:
            type_info = self._register_literal_type(node, py_container)
            for i, el in enumerate(node.elts):
                ti = self.ast_context.lookup_type_info_by_node(el)
                self._assert_resolved_type(ti)
                if ti is None:
                    break
                type_info.register_contained_type(i, ti)

    def compare(self, node, num_children_visited):
        super().compare(node, num_children_visited)
        if num_children_visited == -1:
            assert len(node.comparators) == 1
            self._register_literal_type(node, True) # register boolean type

    def loop_for(self, node, num_children_visited, is_foreach):
        super().loop_for(node, num_children_visited, is_foreach)
        if num_children_visited == 2: # type handling for loop iter/target
            self._register_type_info_by_node(node, context.TypeInfo.notype())
            # this logic is similar to assign, refactor to share
            type_info = self.ast_context.lookup_type_info_by_node(node.iter)
            self._assert_resolved_type(type_info, "cannot lookup for loop target type by iter type %s" % node.iter)
            if type_info is not None:
                contained_type_info = type_info.get_contained_type_info_at(0)
                assert contained_type_info is not None, "don't know how to iterate over %s" % type_info
                if contained_type_info is not None:
                    target = node.target.get()
                    if isinstance(target, ast.Tuple):
                        for i, unpacked_lhs in enumerate(target.elts):
                            ti = contained_type_info.get_contained_type_info_at(i)
                            self._assert_resolved_type(ti, "Unable to lookup contained type of loop target (unpacking) %s" % contained_type_info)
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
        assert declaration_node is not None, "cannot find declaration node for ident %s" % ident_name
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
        if type_info is None:
            # None is allowed because there are multiple visits of the ast to
            # determine all types - initial calls to this method may pass in
            # None
            pass
        else:
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
