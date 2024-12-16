import ast
import copy
import lang.internal.function as funcm
import lang.internal.typeinfo as tim
import lang.nodebuilder as nodebuilder
import lang.nodes as nodes
import lang.scope as scopem
import visitor.nodeattrs as nodeattrs
import visitor.attrresolver as resolverm
import visitor.visitor as visitor
import visitor.visitors as visitors


class TypeVisitor(visitors._CommonStateVisitor):
    """
    This visitor determines the type of most (-> the important) AST Nodes.
    """

    def __init__(self, ast_context, target):
        super().__init__(ast_context, target)
        # resolver for visited functions/methods
        self.resolver = resolverm.AttributeResolver(ast_context.resolver)
        # starts out True, set to False when an unresolved type is encountered
        self.resolved_all_type_references = True
        # maps literal node instance to their TypeInfo instance
        self.literal_node_to_type_info = {}
        # emergency brake
        self.num_visits = 0
        # the artificial global scope for global functions/identifier
        self.global_scope = None

    @property
    def should_revisit(self):
        if self.resolved_all_type_references:
            return False
        else:
            assert self.num_visits < 6, "cannot resolve all references, check method _assert_resolved_type"
            self.resolved_all_type_references = True
            self.num_visits += 1
            return True

    def module(self, node, num_children_visited):
        super().module(node, num_children_visited)
        if num_children_visited == 0:
            # attach special parent scope with global identifiers, such as
            # len, open, sorted ...
            scope = self._get_scope()
            if self.global_scope is None:
                self.global_scope = scopem.Scope(None, node, "global")
            scope.attach_parent(self.global_scope)
            top_level_functions = self.resolver.get_top_level_functions()
            for func in top_level_functions:
                funcdef_node = nodes.build_funcdef_node_for_function(func)
                self.global_scope.register_ident_node(funcdef_node)

    def assign(self, node, num_children_visited):
        super().assign(node, num_children_visited)
        if num_children_visited == -1:
            assert len(node.targets) == 1
            lhs = node.targets[0].get() # get() because array access
            rhs = node.value
            rhs_type_info = self.ast_context.lookup_type_info_by_node(rhs)
            if isinstance(lhs, ast.Subscript):
                self._handle_dict_put(node, lhs, rhs, rhs_type_info)
            else:
                if self._assert_resolved_type(rhs_type_info, "Unable to lookup type of assignment rhs %s, lhs is %s" % (ast.dump(rhs), ast.dump(lhs))):
                    handle_assign = True
                    # check for an edge case below - when we have this pattern:
                    # a = None
                    # a = 1
                    # we have some code running in on_scope_released that sets
                    # the right type on the declaration node
                    # we don't want to blow that type away again here
                    scope = self._get_scope()
                    if scope.is_declaration_node(lhs):
                        lhs_type_info = self.ast_context.lookup_type_info_by_node(lhs)
                        if lhs_type_info is not None and rhs_type_info is not None:
                            if not lhs_type_info.is_none_type and rhs_type_info.is_none_type:
                                # don't reset the lhs type if we already have one!
                                handle_assign = False
                    if handle_assign:
                        self._handle_assign(node, lhs, rhs, rhs_type_info)

    def _handle_assign(self, node, lhs, rhs, rhs_type_info):
        """
        General approach:
        1. a, b = rhs: unpacking, look at contained types for both lhs and ths
                       and register the rhs types as the lhs types
        2. a = b: "regular" assignment, propagate the rhs type info to the lhs
        """
        if isinstance(lhs, ast.Tuple):
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

    def _handle_dict_put(self, node, lhs, rhs, rhs_type_info):
        """
        d[a] = b: register the key's type and the value's type with the
                  dict's type
        """
        assert isinstance(lhs, ast.Subscript)
        key_type_info = None
        value_type_info = None
        target_type_info = self.ast_context.lookup_type_info_by_node(lhs.value)
        if self._assert_resolved_type(target_type_info, "Unable to lookup type of assignment lhs (dict) %s" % lhs.value):
            key_type_info = self.ast_context.lookup_type_info_by_node(lhs.slice)
            self._assert_resolved_type(key_type_info, "Unable to lookup type of assignment lhs (dict key) %s" % lhs.value)

            value_type_info = self.ast_context.lookup_type_info_by_node(rhs)
            if self._assert_resolved_type(value_type_info, "Unable to lookup type of assignment rhs (dict value) %s" % rhs):
                # set the type of this assignment to the value's type
                self._register_type_info_by_node(node, value_type_info)

            if key_type_info is None and target_type_info is None:
                # we don't know types, nothing to do
                pass
            else:
                # we know at least one of the types, register
                cmd = getattr(node, nodeattrs.CONTAINER_MD_ATTR)
                cmd.register(target_type_info, key_type_info, value_type_info)

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
                            # since the slice is an index into an array, we know
                            # it must be an int
                            self._register_type_info_by_node(node.slice, tim.TypeInfo.int())
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
            receiver_type_info = self.ast_context.lookup_type_info_by_node(node.value)
            if self._assert_resolved_type(receiver_type_info, "cannot determine type of receiver instance %s" % ast.dump(node.value)):
                ti = None
                ti = self.resolver.resolve_to_type(receiver_type_info, node.attr)
                if ti is None and False:
                    func_name = node.attr
                    method = self.ast_context.get_method(func_name, receiver_type_info)
                    assert method is not None, "Unknown attr [%s]" % func_name
                    rtn_type_info = method.get_rtn_type_info()
                    ti = rtn_type_info
                if ti is not None:
                    # common case: inst.m1() - this is handled in call()
                    # but real attr access is hanlded here:
                    # os.path.sep
                    self._register_type_info_by_node(node, ti)

    def unaryop(self, node, num_children_visited):
        super().binop(node, num_children_visited)
        if num_children_visited == -1:
            type_info = self.ast_context.lookup_type_info_by_node(node.operand)
            if self._assert_resolved_type(type_info, "unaryop: missing type information for operand %s" % node.operand):
                self._register_type_info_by_node(node, type_info)

    def boolop(self, node, num_children_visited):
        super().boolop(node, num_children_visited)
        if num_children_visited == -1:
            self._register_type_info_by_node(node, tim.TypeInfo.bool())

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
                        target_type_info = tim.TypeInfo(target_type)
                        # required because this call node was materialized
                        # out of thin air and we need to have a return type
                        # for it
                        nodeattrs.set_type_info(conv_node, target_type_info)
                        nodeattrs.set_attr(rhs, nodeattrs.ALT_NODE_ATTR, conv_node)
                else:
                    target_type = self.target.combine_types(
                        lhs_type_info.value_type, rhs_type_info.value_type)
                target_type_info = tim.TypeInfo(target_type)
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
                if self._assert_resolved_type(arg_type_info, "cannot resolve argument type for call of func '%s' for arg node: %s" % (func_name, arg)):
                    if hasattr(arg, nodeattrs.ADDRESS_OF_NODE_MD):
                        # this is not absolutely necessary, but it allows us
                        # to be more strict when we check that all function
                        # invocations use the same types
                        # if we do not do this, the function signature
                        # type is a pointer, but the type at the callsite
                        # (ie this code path) is not a pointer -
                        # for example, the function takes a *string and
                        # we call the function with the arg node "name", which
                        # is a string type, but it has the "address_of_node" md
                        # so that later we emit &name.
                        # so tldr, make this arg node a pointer type
                        arg_type_info = self._ensure_pointer_ti(arg_type_info)
                    arg_type_infos.append(arg_type_info)
                else:
                    break
            else: # nobreak
                target_instance_type_info = None
                is_method = isinstance(node.func, ast.Attribute)
                func = None
                if is_method:
                    receiver_type_info = self.ast_context.lookup_type_info_by_node(node.func.value)
                    if self._assert_resolved_type(receiver_type_info, "cannot resolve the instance [%s] is called on: %s" % (func_name, ast.dump(node.func.value))):
                        func = self.resolver.resolve_to_function(receiver_type_info, func_name)
                        if func is None:
                            # if we don't have an associated method, it must
                            # have been re-written, for example:
                            #    "foo".startswith -> "foo".startsWith
                            # the re-writing propagtes the rtn type anyway
                            # and that's all we care about
                            assert nodeattrs.get_type_info(node) is not None, "resolver doesn't know about %s on %s or bug?" % (func_name, receiver_type_info)
                            
                            # if we already set it on the call node, re-use
                            func = nodeattrs.get_function(node, must_exist=False)
                            if func is None:
                                func = funcm.Function.placeholder(func_name)
                else:
                    ti = self.ast_context.lookup_type_info_by_node(node.func)
                    if ti is not None and ti.is_function:
                        # lambda - the function instance is carried on the
                        # TypeInfo instance
                        func = ti.function
                        assert func is not None
                    else:
                        scope = self._get_scope()
                        decl_node = scope.get_declaration_node(func_name)
                        if decl_node is None:
                            # if we don't have an associated function, it must
                            # have been re-written, for example:
                            #    print -> fmt.Println
                            #    "foo.split..." -> Arrays.asList("foo".split...)
                            # fmt.Println has not been declared in the scope,
                            # but the re-writing propagtes the rtn type anyway
                            # and that's all we care about
                            # we assert here that we do have the rtn type
                            assert nodeattrs.get_type_info(node) is not None
                            func = funcm.Function.placeholder(func_name)
                            n = nodebuilder.funcdef(func_name)
                            nodeattrs.set_function(n, func)
                            self.global_scope.register_ident_node(n)
                        else:
                            # ClassDef for ctor
                            if isinstance(decl_node, (ast.ClassDef, ast.FunctionDef)):
                                func = nodeattrs.get_function(decl_node, must_exist=False)
                            else:
                                # weird edge case - after translating to elisp
                                # we have for loops:
                                # (dolist (s some-list) ...
                                # but s is a ast.Name, not a ast.FunctionDef
                                # it just gets re-written as a func call because
                                # elisp
                                # that's why we check for the node type
                                pass
                if func is not None:
                    self._process_call(node, func, arg_type_infos)

    def _process_call(self, node, func, arg_type_infos):
        assert isinstance(node, ast.AST)
        func.register_invocation(arg_type_infos)

        # handles registering container types, ie list -> list[string]
        if hasattr(node, nodeattrs.CONTAINER_MD_ATTR):
            args = list(arg_type_infos)
            is_method = isinstance(node.func, ast.Attribute)
            if is_method:
                # there's receiver_type_info determination above, combine?
                receiver_type_info = self.ast_context.lookup_type_info_by_node(node.func.value)
                self._assert_resolved_type(receiver_type_info, "cannot lookup container type info %s" % node.func.value)
                args = [receiver_type_info] + args
            cmd = getattr(node, nodeattrs.CONTAINER_MD_ATTR)
            cmd.register(*args)
                
        # propagate the return type of the func to this call node
        if nodeattrs.has_type_info(node):
            # when call nodes are created dynamically during ast rewriting,
            # the return type is associated with the node as type info md
            # examples:
            #   l[0] -> l.get(0): the rtn type depends on the list content
            #   rw.call("String.format", rtn_type=str)
            #   rw.call(print)
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
        self._register_type_info_by_node(node, tim.TypeInfo.notype())

    def cond_if_expr(self, node, num_children_visited):
        super().cond_if_expr(node, num_children_visited)
        if num_children_visited == -1:
            # check both the if and the else branch for type info, otherwise
            # expressions like this won't work:
            # a = 2 if 1 > 0 else None
            if_ti = self.ast_context.lookup_type_info_by_node(node.body)
            else_ti = self.ast_context.lookup_type_info_by_node(node.orelse)
            if self._assert_resolved_type([if_ti, else_ti], "cannot figure out rtn type for if expr %s" % node):
                ti = tim.TypeInfo.get_homogeneous_type([if_ti, else_ti], allow_none_matches=True)
                self._register_type_info_by_node(node, ti)

    def lambdadef(self, node, num_children_visited):
        super().lambdadef(node, num_children_visited)
        func = None
        ti = self.ast_context.lookup_type_info_by_node(node)
        if ti is None:
            func = funcm.Function("lambda")
            func.has_definition = True
            nodeattrs.set_function(node, func, allow_reset=False)
            ti = tim.TypeInfo.function(func)
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

    def classdef(self, node, num_children_visited):
        super().classdef(node, num_children_visited)
        class_type = tim.TypeInfo.clazz(node.name)
        if num_children_visited == 0:
            self._register_type_info_by_node(node, class_type)
        elif num_children_visited == -1:
            func = nodeattrs.get_function(node, must_exist=False)
            if func is None:
                # ctor from the caller's perspective
                func = funcm.Function(class_type.name, class_type)
                func.has_definition = True
                func.is_constructor = True
                nodeattrs.set_function(node, func)
            ctor = self.resolver.resolve_to_function(class_type, "__init__")
            if ctor is not None:
                # ctor from the class' perspective
                ctor.register_rtn_type(class_type)
                ctor.is_constructor = True
                if func.invocation is not None:
                    ctor.register_invocation(func.invocation)

    def funcdef(self, node, num_children_visited):
        super().funcdef(node, num_children_visited)
        scope = self._get_scope()
        class_name = scope.get_enclosing_class_name()
        class_ti = tim.TypeInfo.clazz(class_name) if class_name is not None else None
        func_name = node.name
        if num_children_visited == 0:
            func = nodeattrs.get_function(node, must_exist=False)
            if func is None:
                func = funcm.Function(func_name)
                func.has_definition = True
                nodeattrs.set_function(node, func)
                if class_name is not None:
                    self.resolver.register(class_ti, func)
            func.clear_registered_rtn_type_infos()
            self._register_type_info_by_node(node, tim.TypeInfo.notype())
            if len(node.args.args) > 0:
                self._handle_function_argument_types(node, func, class_ti)
        elif num_children_visited == -1:
            func = nodeattrs.get_function(node, must_exist=True)
            if not func.has_explicit_return:
                func.register_rtn_type(tim.TypeInfo.none())

    def _handle_function_argument_types(self, node, func, enclosing_class_ti=None):
        """
        Named/optional args are not implemeted.
        """
        assert  len(node.args.args) > 0
        arg_nodes_start_index = 0
        if enclosing_class_ti is not None:
            # self
            self._register_type_info_by_node(node.args.args[0].get(), enclosing_class_ti)
            arg_nodes_start_index = 1
        # first we check whether we have types for the func args already
        # this may be possible for some edge cases, for example:
        # def foo(a):
        #   a = 1
        resolved_all_arg_types = True
        func.clear_registered_arg_type_infos()
        for i in range(arg_nodes_start_index, len(node.args.args)):
            arg_node = node.args.args[i].get()
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
                num_signature_args = len(node.args.args) - arg_nodes_start_index
                assert num_signature_args == len(invocation)
                for i, caller_arg_type_info in enumerate(invocation):
                    arg_node = node.args.args[i + arg_nodes_start_index].get()
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
                scope = self._get_scope()
                namespace, ns_node = scope.get_enclosing_namespace()
                assert isinstance(ns_node, (ast.Lambda, ast.FunctionDef)), "unexpected type %s" % ns_node
                if isinstance(ns_node, ast.Lambda):
                    # see lambdadef for rtn type handling
                    pass
                elif isinstance(ns_node, ast.FunctionDef):
                    assert namespace is not None, "return from what?"
                    func = nodeattrs.get_function(ns_node, must_exist=True)
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
            # we need to process the contained types twice: the first time
            # to find out whether they are homogeneous or mixed, based on
            # that we can lookup the right type mapping
            # the type mapping tells us whether the type supports mixed types
            # (this indirection may not be necessary?)
            reg_all_types = True # whether to register all contained types
            # for the first processing, we use a dummy (copy of the) type info
            ti_clone = copy.deepcopy(type_info)
            ok = self._handle_container_elements(node, ti_clone, reg_all_types)
            if ok:
                tm = self.target.type_mapper.get_type_mapping(ti_clone)
                if tm.homogenous_types is not None:
                    # typically, a list cannot have mixed types in statically
                    # typed languages
                    reg_all_types = not tm.homogenous_types
                ok = self._handle_container_elements(node, type_info, reg_all_types)
                assert ok

    def container_type_tuple(self, node, num_children_visited):
        super().container_type_tuple(node, num_children_visited)
        if num_children_visited == -1:
            type_info = self._register_tuple_literal_type(node)
            self._handle_container_elements(node, type_info)
        
    def _handle_container_elements(self, node, type_info, reg_all_types=True):
        """
        Returns a boolean to indicate whether all types resolved.
        """
        for i, el in enumerate(node.elts):
            el = el.get() # req b/c iteration
            ti = self.ast_context.lookup_type_info_by_node(el)
            if self._assert_resolved_type(ti, "_handle_container_elements: cannot determine type of element %s" % ast.dump(el)):
                if reg_all_types or i == 0:
                    type_info.register_contained_type(i, ti)
            else:
                return False
        return True

    def compare(self, node, num_children_visited):
        super().compare(node, num_children_visited)
        if num_children_visited == -1:
            assert len(node.comparators) == 1
            self._register_literal_type(node, True) # register boolean type

    def loop_for(self, node, num_children_visited, is_foreach):
        super().loop_for(node, num_children_visited, is_foreach)
        if num_children_visited == 2 and is_foreach:
            # type handling for loop iter/target
            self._register_type_info_by_node(node, tim.TypeInfo.notype())
            # this logic is similar to assign, refactor to share
            type_info = self.ast_context.lookup_type_info_by_node(node.iter)
            if self._assert_resolved_type(type_info, "cannot lookup for loop target type by iter type %s" % ast.dump(node.iter)):
                # some of this needs to also happen in the astrewriter
                contained_type_info = type_info.get_contained_type_info_at(0)
                if self._assert_resolved_type(contained_type_info, "Unable to get the type of the iter variable %s" % node.iter):
                    target = node.target
                    if isinstance(target, ast.Tuple):
                        for i, unpacked_lhs in enumerate(target.elts):
                            ti = contained_type_info.get_contained_type_info_at(i)
                            if self._assert_resolved_type(ti, "Unable to lookup contained type of loop target (unpacking) %s" % contained_type_info):
                                self._register_type_info_by_node(unpacked_lhs, ti)
                    else:
                        self._register_type_info_by_node(target, contained_type_info)

    def name(self, node, num_children_visited):
        """
        Can't this just generically look at whether name is in scope or not -
        and if it is, get the type.
        """
        super().name(node, num_children_visited)
        scope = self._get_scope()
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
        super().import_stmt(node, num_children_visited)
        if num_children_visited == 0:
            scope = self._get_scope()
            for alias_node in node.names:
                imported_module = alias_node.name
                self._register_type_info_by_node(alias_node, tim.TypeInfo.module(imported_module))

    def on_scope_released(self, scope):
        super().on_scope_released(scope)
        types_resolved = self._update_declaration_type_info(scope)
        if types_resolved:
            self._update_name_nodes_type_info(scope)
        if not self.target.dynamically_typed:
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

          s := foo()
          s = d["key"]
          -> the declaration identifier is a pointer, the identifier then
             holds a different value of the same type, but the rhs is not a
             pointer

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
                    elif decl_type_info.is_equal_ignoring_pointers(ident_type_info):
                        if decl_type_info != ident_type_info:
                            # here, the decl type and the ident type only
                            # differ in their pointerness
                            # this can happen like this:
                            # name := get_str() # returns a pointer
                            # team-name = d["foo"] # map[string]string
                            self._register_type_info_by_node(ident_node, decl_type_info)
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
                    try:
                        tim.TypeInfo.get_homogeneous_type(
                            [ident_type_info, decl_type_info],
                            allow_none_matches=True)
                    except Exception as e:
                        raise Exception("Error while checking types for ident: [%s]" % ident_name) from e

    def _get_declaration_node_for_ident_name(self, ident_name, scope=None, must_exist=True):
        if scope is None:
            scope = self._get_scope()
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
            elif isinstance(t, tim.TypeInfo):
                if t.is_none_type:
                    if not allow_none_type:
                        # print("DEBUG Got NoneType: %s" % msg)
                        self.resolved_all_type_references = False
                        break
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
            assert type_info is None or isinstance(type_info, tim.TypeInfo), "unexpected type %s" % type_info
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
            type_info = tim.TypeInfo.none()
        else:
            type_info = tim.TypeInfo(type(value))
        self._register_type_info_by_node(node, type_info)
        return type_info

    def _get_scope(self):
        return self.ast_context.current_scope.get()
