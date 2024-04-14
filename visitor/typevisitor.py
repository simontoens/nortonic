import ast

from visitor import visitor
from visitor import visitors
import copy
import context
import nodeattrs
import nodebuilder


class TypeVisitor(visitors._CommonStateVisitor):
    """
    This visitor determines the type of most AST Node (the important ones).

    It also create Function instances and attaches them to Funcdef and Call
    nodes.
    """

    def __init__(self, ast_context, target):
        super().__init__(ast_context, target)
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
            lhs = node.targets[0].get() # get() because array access
            rhs = node.value
            rhs_type_info = self.ast_context.lookup_type_info_by_node(rhs)
            if self._assert_resolved_type(rhs_type_info, "Unable to lookup type of assignment rhs %s, lhs is %s" % (ast.dump(rhs), ast.dump(lhs))):

                handle_assign = True
                # check for an edge case below - when we have this pattern:
                # a = None
                # a = 1
                # we have some code running in on_scope_released that sets
                # the right type on the declaration node
                # we don't want to blow that type away again here
                scope = self.ast_context.current_scope.get()
                if scope.is_declaration_node(lhs):
                    lhs_type_info = self.ast_context.lookup_type_info_by_node(lhs)
                    if lhs_type_info is not None and rhs_type_info is not None:
                        if not lhs_type_info.is_none_type and rhs_type_info.is_none_type:
                            # don't reset the lhs type if we already have one!
                            handle_assign = False
                if handle_assign:
                    self._handle_assign(node, lhs, rhs, rhs_type_info)

            # scope = self.ast_context.current_scope.get()
            # if scope.is_declaration_node(lhs):
            #     lhs_type_info = self.ast_context.lookup_type_info_by_node(lhs)
            #     if lhs_type_info is not None and rhs_type_info is not None:
            #         if not lhs_type_info.is_none_type and rhs_type_info.is_none_type:
            #             return
                    # ti = context.TypeInfo.get_homogeneous_type([lhs_type_info, rhs_type_info], allow_none_matches=True)
                    # if not ti.is_none_type:
                    #     pass
                    #     #return
            
    def _handle_assign(self, node, lhs, rhs, rhs_type_info):
        """
        General approach:
        1. d[a] = b: register the key's type and the value's type with the
                     dict's type
        2. a, b = rhs: unpacking, look at contained types for both lhs and ths
                       and register the rhs types as the lhs types
        3. a = b: "regular" assignment, propagate the rhs type info to the lhs
        """
        if isinstance(lhs, ast.Subscript):
            # d["foo"] = blah - special syntax
            # (this same check exists in most visitors)
            # register the type being added to the dict:
            lhs_type_info = self.ast_context.lookup_type_info_by_node(lhs.value)
            if self._assert_resolved_type(lhs_type_info, "Unable to lookup type of assignment lhs (subscript) %s" % lhs.value):
                key_type_info = self.ast_context.get_type_info_by_node(lhs.slice)
                self._register_type_info_by_node(node, context.TypeInfo.notype())

                cmd = getattr(node, nodeattrs.CONTAINER_MD_ATTR)
                cmd.register(lhs_type_info, key_type_info, rhs_type_info)

        elif isinstance(lhs, ast.Tuple):
            for i, unpacked_lhs in enumerate(lhs.elts):
                ti = rhs_type_info.get_contained_type_info_at(i)
                if self._assert_resolved_type(ti, "Unable to lookup contained type of assigned rhs (unpacking) %s" % rhs_type_info):
                    self._register_type_info_by_node(unpacked_lhs, ti)
        else:
            self._register_type_info_by_node(lhs, rhs_type_info)
            # this isn't technically correct, assignment isn't an
            # expression that evaluates to a value on Python
            # (but for elisp this would be correct)
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
                    if type_info.is_sequence:
                        if isinstance(node.slice, ast.Constant):
                            # if index is by constant number, ie a = l[2],
                            # look up specific type at that slot
                            contained_type_info = type_info.get_contained_type_info_at(node.slice.value)
                        else:
                            assert isinstance(node.slice, ast.Name)
                            self._register_type_info_by_node(node.slice, context.TypeInfo.int())
                            # get the type of the first element and hope for the best
                            contained_type_info = type_info.get_contained_type_info_at(0)
                    else:
                        # FIXME assumes dict - need to check target type
                        contained_type_info = type_info.get_contained_type_info_at(1)
                    if self._assert_resolved_type(contained_type_info, "cannot lookup contained type of subscript expression %s" % ast.dump(node.value)):
                        self._register_type_info_by_node(node, contained_type_info)

    def assign_aug(self, node, num_children_visited):
        super().assign_aug(node, num_children_visited)
        if num_children_visited == -1:
            type_info = self.ast_context.lookup_type_info_by_node(node.value)
            if self._assert_resolved_type(type_info, "cannot lookup type of assign_aug node.value %s" % node.value):
                #assert False, "%s %s" % (type_info, ast.dump(node))
                self._register_type_info_by_node(node, type_info)
                    
    def attr(self, node, num_children_visited):
        super().attr(node, num_children_visited)
        if num_children_visited == -1:
            # foo.blah() -> the type of foo
            target_instance_type_info = self.ast_context.lookup_type_info_by_node(node.value)
            # 1. os.path - value is "os", attr is "path", so module target
            # 2. os.path.set - value is (os.path), attr is sep (str), but cannot lookup target because we didn't register it in step 1?
            if self._assert_resolved_type(target_instance_type_info, "cannot determine type of target instance %s" % ast.dump(node.value)):
                func_name = node.attr
                method = self.ast_context.get_method(func_name, target_instance_type_info)
                assert method is not None, "Unknown attr [%s]" % func_name
                rtn_type_info = method.get_rtn_type_info()
                if rtn_type_info is not None:
                    # common case: inst.m1() - this is handled in call()
                    # but real attr access is hanlded here:
                    # os.path.sep
                    self._register_type_info_by_node(node, rtn_type_info)

    def unaryop(self, node, num_children_visited):
        super().binop(node, num_children_visited)
        if num_children_visited == -1:
            type_info = self.ast_context.lookup_type_info_by_node(node.operand)
            if self._assert_resolved_type(type_info, "unaryop: missing type information for operand %s" % node.operand):
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
                    coercion_fname = coercion_rule.rhs_conversion_function_name
                    if coercion_fname is not None:
                        # this should be in astrewriter?
                        arg_node = copy.copy(rhs)
                        conv_node = nodebuilder.call(coercion_fname, [arg_node])
                        target_type_info = context.TypeInfo(target_type)
                        # required because this call node was materialized
                        # out of thin air and we need to have a return type
                        # for it
                        nodeattrs.set_type_info(conv_node, target_type_info)
                        nodeattrs.set_attr(rhs, nodeattrs.ALT_NODE_ATTR, conv_node)
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
            arg_type_infos = []
            for arg in node.args:
                arg = arg.get() # required because of iteration
                arg_type_info = nodeattrs.get_type_info(arg)
                if arg_type_info is None:
                    arg_type_info = self.ast_context.lookup_type_info_by_node(arg)
                arg_type_infos.append(arg_type_info)
            if self._assert_resolved_type(arg_type_infos, "cannot resolve all argument types for call of func '%s': %s" % (func_name, arg_type_infos)):        
                target_instance_type_info = None
                is_method = isinstance(node.func, ast.Attribute)
                func = None
                if is_method:
                    target_instance_type_info = self.ast_context.lookup_type_info_by_node(node.func.value)
                    if self._assert_resolved_type(target_instance_type_info, "cannot resolve the instance [%s] is called on: %s" % (func_name, ast.dump(node.func.value))):
                        func = self.ast_context.get_method(func_name, target_instance_type_info)
                else:
                    ti = self.ast_context.lookup_type_info_by_node(node.func)
                    if ti is not None and ti.is_function:
                        # lambda - the function instance is carried on the
                        # TypeInfo instance
                        func = ti.function
                        assert func is not None
                    else:
                        func = self.ast_context.get_function(func_name)
                if func is None:
                    # can happen if above we don't have a
                    # target_instance_type_info yet
                    pass
                else:
                    self._process_call(node, func, arg_type_infos)

    def _process_call(self, node, func, arg_type_infos):
        # append -> methods that populate the target container
        append_methods = ["add", "put",]
        append_functions = ["append", "add-to-list", "puthash"]
        all_append_names = append_methods + append_functions
        assert isinstance(node, ast.AST)
        assert isinstance(func, context.Function)
        func.register_invocation(arg_type_infos)

        # handles registering container types, ie list -> list[string]
        if hasattr(node, nodeattrs.CONTAINER_MD_ATTR):
            args = list(arg_type_infos)
            if func.target_instance_type_info is not None:
                target_instance_type_info = self.ast_context.lookup_type_info_by_node(node.func.value)
                self._assert_resolved_type(target_instance_type_info, "Cannot lookup container type info %s" % node.func.value)
                args = [target_instance_type_info] + args
            if None not in args:
                cmd = getattr(node, nodeattrs.CONTAINER_MD_ATTR)
                cmd.register(*args)
                
        # propagate the return type of the func to this call node
        if nodeattrs.has_type_info(node):
            # when call nodes are created dynamically during ast rewriting,
            # the return type is associated with the node as type info md
            # examples:
            #   l[0] -> l.get(0): the rtn type depends on the list content
            #   rw.call("String.format", rtn_type=str)
            #   rw.call(context.PRINT_BUILTIN)
            #   len("foo") -> "foo".length()
            rtn_type_info = nodeattrs.get_type_info(node)
        else:
            rtn_type_info = func.get_rtn_type_info()

        if self._assert_resolved_type(rtn_type_info, "no rtn type for func %s %s" % (func.name, node.func)):
            if rtn_type_info.has_late_resolver:
                rtn_type_info = rtn_type_info.apply_late_resolver(arg_type_infos[0])
            self.ast_context.register_type_info_by_node(node, rtn_type_info)

        # for other visitors, for example CallsiteVisitor
        allow_reset = func.name == "lambda" # hack
        nodeattrs.set_function(node, func, allow_reset)

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

    def lambdadef(self, node, num_children_visited):
        super().lambdadef(node, num_children_visited)
        func = None
        ti = self.ast_context.lookup_type_info_by_node(node)
        if ti is None:
            func = context.Function("lambda")
            func.has_definition = True
            nodeattrs.set_function(node, func, allow_reset=False)
            ti = context.TypeInfo.function(func)
            self.ast_context.register_type_info_by_node(node, ti)
        else:
            func = ti.function
        if num_children_visited == -1:
            rtn_ti = nodeattrs.get_type_info(node.body)
            if rtn_ti is None:
                rtn_ti = self.ast_context.lookup_type_info_by_node(node.body)
            if self._assert_resolved_type(rtn_ti, "cannot get lambda return type %s" % ast.dump(node)):
                func.register_rtn_type(rtn_ti)
            if len(node.args.args) > 0:
                self._handle_function_argument_types(node, func)
                
    def funcdef(self, node, num_children_visited):
        super().funcdef(node, num_children_visited)
        func_name = node.name
        func = self.ast_context.get_function(func_name)
        func.has_definition = True
        # needed for PointerVisitor and TokenVisitor
        nodeattrs.set_function(node, func)
        if num_children_visited == 0:
            self._register_type_info_by_node(node, context.TypeInfo.notype())
            if len(node.args.args) > 0:
                self._handle_function_argument_types(node, func)
        elif num_children_visited == -1:
            if not func.has_explicit_return:
                func.register_rtn_type(context.TypeInfo.none())

    def _handle_function_argument_types(self, node, func):
        """
        FIXME - only positional args are supported
        """
        assert  len(node.args.args) > 0

        # first we check whether we have types for the func args already
        # this may be possible for some edge cases, for example:
        # def foo(a):
        #   a = 1
        resolved_all_arg_types = True
        func.clear_registered_arg_type_infos()        
        for arg_node in node.args.args:
            ti = self.ast_context.lookup_type_info_by_node(arg_node)
            # if we also have invocations, we can check here that the types
            # match?
            if ti is None:
                resolved_all_arg_types = False
                break
            else:
                if nodeattrs.get_attr(arg_node, nodeattrs.IS_POINTER_NODE_ATTR):
                    ti = self._ensure_pointer_ti(ti)
                    self._register_type_info_by_node(arg_node, ti)
                func.register_arg_type_info(ti)
                
        if not resolved_all_arg_types:
            # lookup previous invocation to determine the argument types
            invocation = func.invocation
            if self._assert_resolved_type(invocation, "cannot find invocation of function %s" % ast.dump(node, indent=2), allow_none_type=False):

                assert len(node.args.args) == len(invocation)
                for i, caller_arg_type_info in enumerate(invocation):
                    arg_node = node.args.args[i]
                    funcdef_arg_type_info = caller_arg_type_info
                    if nodeattrs.get_attr(arg_node, nodeattrs.IS_POINTER_NODE_ATTR):
                        funcdef_arg_type_info = self._ensure_pointer_ti(funcdef_arg_type_info)
                    self._register_type_info_by_node(arg_node, funcdef_arg_type_info)
                    func.register_arg_type_info(funcdef_arg_type_info)

    def rtn(self, node, num_children_visited):
        super().rtn(node, num_children_visited)
        if num_children_visited == -1:
            rtn_type_info = self.ast_context.lookup_type_info_by_node(node.value)
            if self._assert_resolved_type(rtn_type_info, "cannot lookup rtn type info by node type %s" % node.value):
                if nodeattrs.get_attr(node, nodeattrs.IS_POINTER_NODE_ATTR):
                    rtn_type_info = self._ensure_pointer_ti(rtn_type_info)
                self._register_type_info_by_node(node, rtn_type_info)
                scope = self.ast_context.current_scope.get()
                namespace, ns_node = scope.get_enclosing_namespace()
                assert isinstance(ns_node, (ast.Lambda, ast.FunctionDef)), "unexpected type %s" % ns_node
                if isinstance(ns_node, ast.Lambda):
                    # see lambdadef for rtn type handling
                    pass
                elif isinstance(ns_node, ast.FunctionDef):
                    assert namespace is not None, "return from what?"
                    func = self.ast_context.get_function(namespace)
                    func.has_explicit_return = True
                    func.register_rtn_type(rtn_type_info)
                    func.returns_literal = not isinstance(node.value, ast.Name)

    def _ensure_pointer_ti(self, org_type_info):
        if org_type_info.is_pointer:
            return org_type_info
        else:
            ti_copy = copy.deepcopy(org_type_info)
            ti_copy.is_pointer = True
            ti_copy.backing_type_info = org_type_info
            ti_copy.is_pointer = True
            return ti_copy

    def container_type_dict(self, node, num_children_visited):
        super().container_type_dict(node, num_children_visited)
        if num_children_visited == -1:
            # instead of re-creating the TypeInfo instance from scratch,
            # we first try to find the one we created previously
            # this is necessary when this typevisitor runs after ast rewrites
            # have been applied, because the logic we have to register
            # contained types won't fire anymore (since typically Python
            # dict interaction syntax is replaced by function calls)            
            dict_type_info = self.ast_context.lookup_type_info_by_node(node)
            if dict_type_info is None:
                dict_type_info = self._register_literal_type(node, {})
            assert len(node.keys) == len(node.values)
            ctx = self.ast_context
            for i in range(0, len(node.keys)):
                key_node = node.keys[0]
                key_type_info = ctx.lookup_type_info_by_node(key_node)
                if self._assert_resolved_type(key_type_info, "container_type_dict: cannot lookup key type: %s" % ast.dump(key_node)):
                    dict_type_info.register_contained_type(0, key_type_info)
                value_node = node.values[0]
                value_type_info = ctx.lookup_type_info_by_node(value_node)
                if self._assert_resolved_type(value_type_info, "container_type_dict: cannot lookup value type: %s" % ast.dump(value_node)):
                    dict_type_info.register_contained_type(1, value_type_info)
            
    def container_type_list(self, node, num_children_visited):
        super().container_type_list(node, num_children_visited)
        if num_children_visited == -1:
            type_info = self._register_list_literal_type(node)
            self._handle_container_elements(node, type_info)

    def container_type_tuple(self, node, num_children_visited):
        super().container_type_tuple(node, num_children_visited)
        if num_children_visited == -1:
            type_info = self._register_tuple_literal_type(node)
            self._handle_container_elements(node, type_info)
        
    def _handle_container_elements(self, node, type_info):
        for i, el in enumerate(node.elts):
            el = el.get() # req b/c iteration
            ti = self.ast_context.lookup_type_info_by_node(el)
            if self._assert_resolved_type(ti, "_handle_container_elements: cannot determine type of element %s" % ast.dump(el)):
                type_info.register_contained_type(i, ti)
            else:
                break

    def compare(self, node, num_children_visited):
        super().compare(node, num_children_visited)
        if num_children_visited == -1:
            assert len(node.comparators) == 1
            self._register_literal_type(node, True) # register boolean type

    def loop_for(self, node, num_children_visited, is_foreach):
        super().loop_for(node, num_children_visited, is_foreach)
        if num_children_visited == 2 and is_foreach:
            # type handling for loop iter/target
            self._register_type_info_by_node(node, context.TypeInfo.notype())
            # this logic is similar to assign, refactor to share
            type_info = self.ast_context.lookup_type_info_by_node(node.iter)
            self._assert_resolved_type(type_info, "cannot lookup for loop target type by iter type %s" % ast.dump(node.iter))
            if type_info is not None:
                # some of this needs to also happen in the astrewriter
                contained_type_info = type_info.get_contained_type_info_at(0)
                assert contained_type_info is not None, "don't know how to iterate over %s" % type_info
                if contained_type_info is not None:
                    target = node.target
                    if isinstance(target, ast.Tuple):
                        for i, unpacked_lhs in enumerate(target.elts):
                            ti = contained_type_info.get_contained_type_info_at(i)
                            self._assert_resolved_type(ti, "Unable to lookup contained type of loop target (unpacking) %s" % contained_type_info)
                            self._register_type_info_by_node(unpacked_lhs, ti)
                    else:
                        self._register_type_info_by_node(target, contained_type_info)

    def name(self, node, num_children_visited):
        super().name(node, num_children_visited)
        scope = self.ast_context.current_scope.get()
        if self.visiting_func:
            type_info = self._lookup_type_info_by_ident_name(
                node.id, scope, must_exist=False)
            if type_info is not None and type_info.is_function:                
                # this identifier is a lambda, we need to propagate the type
                # f = lambda: 1
                # f()
                #
                # this is different for function declared globally:
                # print("abc") - print doesn't have a TypeInfo here
                # would be nice to make this consistent at some point?
                self._register_type_info_by_node(node, type_info)
        elif self.assign_visiting_lhs:
            pass
        elif self.loop_visiting_lhs:
            pass
        else:
            # a.startswith, print(a), foo = a, ... - lookup a's type
            # getting the type info from the node first is required for
            # nodes that are created by the astrewriter - they will have an
            # attached type info - for example
            #   .prepend_arg(rw.unresolved_ident(
            #       "identity", nodeattrs.QUOTE_NODE_ATTR)))
            #   the name ident created by: reassign_to_arg
            type_info = nodeattrs.get_type_info(node)
            if type_info is None:
                type_info = self._lookup_type_info_by_ident_name(
                    node.id, scope, must_exist=True)
            if self._assert_resolved_type(type_info, "cannot find type info for ident '%s'" % node.id):
                self._register_type_info_by_node(node, type_info)

    def constant(self, node, num_children_visited):
        super().constant(node, num_children_visited)
        #if not self.ast_context.has_type_info(node):
        self._register_literal_type(node, node.value)

    def expr(self, node, num_children_visited):
        super().expr(node, num_children_visited)
        if num_children_visited == -1:
            value_node = node.value
            type_info = self.ast_context.lookup_type_info_by_node(value_node)
            if self._assert_resolved_type(type_info, "cannot find type info for expr value node %s '%s'" % (id(value_node), ast.dump(value_node))):
                self._register_type_info_by_node(node, type_info)

    def import_stmt(self, node, num_children_visited):
        if num_children_visited == 0:
            super().import_stmt(node, num_children_visited)
            for alias_node in node.names:
                self._register_type_info_by_node(alias_node, context.TypeInfo.module(alias_node.name))

    def on_scope_released(self, scope):
        super().on_scope_released(scope)
        types_resolved = self._update_declaration_type_info(scope)
        if types_resolved:
            self._update_name_nodes_type_info(scope)
        self._detect_mixed_type_assignments(scope)

    def _update_declaration_type_info(self, scope):
        """
        This method handles these cases:
          a = None
          a = 1
          -> the declaration is an int

          l = []
          l = [2]
          -> the declaration node is a list[int]

          (dolist (s l)
          -> the declaration node for 's' has an associated type that was
             not registered (see long comment in code below)

        Returns True if all types resolved, False otherwise.
        """
        types_resolved = True
        for ident_name in scope.get_identifiers_in_this_scope():
            decl_node = self._get_declaration_node_for_ident_name(ident_name, scope)
            decl_type_info = self.ast_context.lookup_type_info_by_node(decl_node)
            if nodeattrs.has_type_info(decl_node):
                if decl_type_info is None:
                    decl_type_info = nodeattrs.get_type_info(decl_node)
                    # this is a bit weird and covers an edge case with elisp:
                    # (dolist (s l): here s is the decl node, but it was
                    # re-rewritten as a call node. it was properly registered
                    # as the decl node, but the type info set on it was never
                    # registered
                    # a better way to implement this would be a generic method
                    # process(node) called for every node in the ast
                    self._register_type_info_by_node(decl_node, decl_type_info)
                else:
                    assert decl_type_info is nodeattrs.get_type_info(decl_node), "fun: type info mismatch to debug"
            
            self._assert_resolved_type(decl_type_info, "on_scope_release cannot lookup type of declaration node for %s" % ident_name)

            for ident_node in scope.get_identifier_nodes_in_this_scope(ident_name):
                if decl_node is ident_node:
                    continue

                ident_type_info = self.ast_context.lookup_type_info_by_node(ident_node)

                self._assert_resolved_type(ident_type_info, "on_scope_release cannot lookup tpye of ident node %s %s" % (ident_name, ident_node))

                if decl_type_info is not None and ident_type_info is not None:
                    types_resolved &= True
                    if decl_type_info.is_none_type:
                        # for this case, we need to fix the type info of the
                        # declaration node:
                        # a = None
                        # a = 1
                        self._register_type_info_by_node(decl_node, ident_type_info)
                    elif decl_type_info.is_container and decl_type_info.num_contained_type_infos == 0:
                        # l = []
                        # l = [2]
                        # register the contained type infos on the decl node
                        decl_type_info.propagate_contained_type_infos(ident_type_info)
                elif decl_type_info is None and ident_type_info is not None:
                    # edge case (another one):
                    # def foo(i):
                    #   l = [1,2,3]
                    #   return l[i]
                    # i is an int, there's no type for the decl node because
                    # there's no invocaton of the function foo
                    self._register_type_info_by_node(decl_node, ident_type_info)
                else:
                    types_resolved = False
        return types_resolved

    def _update_name_nodes_type_info(self, scope):
        """
        This method handles these cases:

        1.
          l = ["foo"]
          l = []
          -> the type of the 2nd l name node is list[string]
        """
        for ident_name in scope.get_identifiers_in_this_scope():
            decl_node = self._get_declaration_node_for_ident_name(ident_name, scope)
            for ident_node in scope.get_identifier_nodes_in_this_scope(ident_name):
                if decl_node is ident_node:
                    continue

                decl_type_info = self.ast_context.get_type_info_by_node(decl_node)
                ident_type_info = self.ast_context.get_type_info_by_node(ident_node)

                if ident_type_info.is_container and ident_type_info.num_contained_type_infos == 0:
                    # this is case 1 (see above)
                    # register the contained type infos of the declaration node
                    # on the ident node
                    ident_type_info.propagate_contained_type_infos(decl_type_info)

    def _detect_mixed_type_assignments(self, scope):
        for ident_name in scope.get_identifiers_in_this_scope():
            for ident_node in scope.get_identifier_nodes_in_this_scope(ident_name):
                ident_type_info = self.ast_context.lookup_type_info_by_node(ident_node)
                decl_node = self._get_declaration_node_for_ident_name(ident_name, scope)
                decl_type_info = self.ast_context.lookup_type_info_by_node(decl_node)
                if ident_type_info is not None and decl_type_info is not None:
                    if ident_type_info.value_type is not decl_type_info.value_type:
                        assert self.target.dynamically_typed, "ident [%s] cannot be both a %s and a %s" % (ident_name, ident_type_info, decl_type_info)

    def _get_declaration_node_for_ident_name(self, ident_name, scope=None, must_exist=True):
        if scope is None:
            scope = self.ast_context.current_scope.get()
        declaration_node = scope.get_declaration_node(ident_name)
        assert not must_exist or declaration_node is not None, "cannot find declaration node for ident [%s]" % ident_name
        return declaration_node

    def _lookup_type_info_by_ident_name(self, ident_name, scope=None, must_exist=True):
        declaration_node = self._get_declaration_node_for_ident_name(ident_name, scope, must_exist)
        return self.ast_context.lookup_type_info_by_node(declaration_node)

    def _assert_resolved_type(self, type_thing, msg, allow_none_type=True):
        if not isinstance(type_thing, (list, tuple)):
            # multiple types can optionally be passed in
            type_thing = [type_thing]
        # when all types have been determined, type_thing should not be None
        for t in type_thing:
            if t is None:
                # print("DEBUG Got None: %s" % msg)
                self.resolved_all_type_references = False
                break
            elif isinstance(t, context.TypeInfo):
                if t.is_none_type:
                    if not allow_none_type:
                        # print("DEBUG Got NoneType: %s" % msg)
                        self.resolved_all_type_references = False
                        break
                elif t.is_container:
                    for ct in t.get_contained_type_infos():
                        # not sure this is really useful, we don't know
                        # what contained type infos have been registered at this
                        # point - additionally, None types should never be
                        # registered in the first place - finally, this needs
                        # to recurse
                        assert ct is not None, "why is there a None contained type?"
                        assert not ct.is_none_type, "why is there a NoneType contained type?"
            else:
                raise AssertionError("Unknown type: %s" % type_thing)
        else:
            return True
        return False

    def _register_type_info_by_node(self, node, type_info):
        assert type_info is not None
        assert type_info.value_type is not None
        if type_info is None:
            # None is allowed because there are multiple visits of the ast to
            # determine all types - initial calls to this method may pass in
            # None
            pass
        else:
            assert type_info is None or isinstance(type_info, context.TypeInfo), "unexpected type %s" % type_info
            self.ast_context.register_type_info_by_node(node, type_info)

    def _register_list_literal_type(self, node):
        # this is needed for lists because its generic type
        # (ie the type of the contained elements) may not be known
        # right away, and needs to be set on the same TypeInfo instance as
        # it is being discovered
        return self._register_literal_type_with_cached_type_info(node, [])

    def _register_tuple_literal_type(self, node):
        # this is needed for tuples because they are the lhs of unpacking
        # they need to be "slowly" populated as function return types are
        # discovered
        return self._register_literal_type_with_cached_type_info(node, ())

    def _register_literal_type_with_cached_type_info(self, node, literal_value):
        # this is needed for tuples because they are the lhs of unpacking
        # they need to be "slowly" populated as function return types are
        # discovered
        if node in self.literal_node_to_type_info:
            type_info = self.literal_node_to_type_info[node]
        else:
            type_info = self._register_literal_type(node, literal_value)
            self.literal_node_to_type_info[node] = type_info
        return type_info

    def _register_literal_type(self, node, value):
        if value is None:
            # this is tricky, because None doesn't cary any useful type
            # information
            # A None literal becomes a NoneType TypeInfo
            # This is different from a "None TypeInfo", which means we don't
            # have any type information at all
            type_info = context.TypeInfo.none()
        else:
            type_info = context.TypeInfo(type(value))
        self._register_type_info_by_node(node, type_info)
        return type_info
