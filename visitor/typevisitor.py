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
        """
        General approach:
        1. d[a] = b: register the key's type and the value's type with the
                     dict's type
        2. a, b = rhs: unpacking, look at contained types for both lhs and ths
                       and register the rhs types as the lhs types
        3. a = b: "regular" assignment, propagate the rhs type info to the lhs
        """
        super().assign(node, num_children_visited)
        if num_children_visited == -1:
            assert len(node.targets) == 1
            lhs = node.targets[0].get()
            rhs = node.value.get()
            if False: #nodeattrs.has_type_info(lhs):
                # TRY TO REMOVE THIS
                # if we associated a type info with the declaration node,
                # we use it, it takes precedence
                rhs_type_info = nodeattrs.get_type_info(lhs)
                self.ast_context.register_type_info_by_node(rhs, rhs_type_info)
            else:
                rhs_type_info = self.ast_context.lookup_type_info_by_node(rhs)
            if isinstance(lhs, ast.Subscript):
                # d["foo"] = blah - special syntax
                # (this same check exists in most visitors)
                # register the type being added to the dict:
                lhs_type_info = self.ast_context.lookup_type_info_by_node(lhs.value)
                if self._assert_resolved_type(lhs_type_info, "Unable to lookup type of assignment lhs (subscript) %s" % lhs.value):
                    key_type_info = self.ast_context.get_type_info_by_node(lhs.slice)
                    lhs_type_info.register_contained_type(0, key_type_info)
                    lhs_type_info.register_contained_type(1, rhs_type_info)
                    self._register_type_info_by_node(node, context.TypeInfo.notype())
            elif isinstance(lhs, ast.Tuple):
                if rhs_type_info is not None:
                    for i, unpacked_lhs in enumerate(lhs.elts):
                        ti = rhs_type_info.get_contained_type_info_at(i)
                        self._assert_resolved_type(ti, "Unable to lookup contained type of assigned rhs (unpacking) %s" % rhs_type_info)
                        self._register_type_info_by_node(unpacked_lhs, ti)
            else:
                if self._assert_resolved_type(rhs_type_info, "Unable to lookup type of assignment rhs %s, lhs is %s" % (ast.dump(rhs), ast.dump(lhs))):
                    self._register_type_info_by_node(lhs, rhs_type_info)
                    # this isn't technically correct, assignment isn't an
                    # expression that evaluates to a value on Python
                    # (but for elisp this would be correct)
                    self._register_type_info_by_node(node, rhs_type_info)

    def subscript(self, node, num_children_visited):
        super().subscript(node, num_children_visited)
        if num_children_visited == -1:
            type_info = self.ast_context.lookup_type_info_by_node(node.value.get())
            self._assert_resolved_type(type_info, "cannot lookup type of subscript node.value %s" % node.value.get())
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
                    if self._assert_resolved_type(contained_type_info, "cannot lookup contained type of subscript expression %s" % ast.dump(node.value.get())):
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
            target_instance_type_info = self.ast_context.lookup_type_info_by_node(node.value.get())
            # 1. os.path - value is "os", attr is "path", so module target
            # 2. os.path.set - value is (os.path), attr is sep (str), but cannot lookup target because we didn't register it in step 1?
            if self._assert_resolved_type(target_instance_type_info, "cannot determine type of target instance %s" % ast.dump(node.value.get())):
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
                        # this should be in astrewriter?
                        arg_node = copy.copy(rhs)
                        conv_node = nodebuilder.call(coercion_fname, [arg_node])
                        target_type_info = context.TypeInfo(target_type)
                        # required because this call node was materialized
                        # out of thin air and we need to have a return type
                        # for it
                        nodeattrs.set_type_info(conv_node, target_type_info)
                        nodeattrs.set_attr(rhs, nodeattrs.ALT_NODE_ATTR, conv_node)
                        #self._register_type_info_by_node(conv_node, target_type_info)
                        # register a function for this coercion rule so that
                        # when this visitor runs for the 2nd time, we can go
                        # through the regular code path that expects a function
                        # instance with a rtn type to exist (see call())
                        #func = self.ast_context.get_function(coercion_fname)
                        #func.register_rtn_type(target_type_info)
                        # TODO - review why this is needed
                        #func._is_builtin = True
                else:
                    target_type = self.target.combine_types(
                        lhs_type_info.value_type, rhs_type_info.value_type)
                target_type_info = context.TypeInfo(target_type)
                self._register_type_info_by_node(node, target_type_info)

    def call(self, node, num_children_visited):
        func_name = super().call(node, num_children_visited)
        if num_children_visited == -1:
            # special case - assignment was rewritten
            # (share code with assign I think?)
            # lhs, rhs = nodeattrs.get_assignment_lhs_rhs_nodes(node)
            # if lhs is not None and rhs is not None:
            #     assert isinstance(lhs, ast.Name)
            #     rhs_type_info = self.ast_context.lookup_type_info_by_node(rhs)
            #     if self._assert_resolved_type(rhs_type_info, "cannot lookup rhs type info %s" % rhs_type_info):
            #         self._register_type_info_by_node(lhs, rhs_type_info)
            #         return
            
            # record this function invocation so we know the argument types
            # it is called with
            arg_type_infos = [self.ast_context.lookup_type_info_by_node(a.get()) for a in node.args]
            if self._assert_resolved_type(arg_type_infos, "cannot resolve all argument types for call of func '%s': %s" % (func_name, arg_type_infos)):        
                target_instance_type_info = None
                is_method = isinstance(node.func, ast.Attribute)
                func = None
                if is_method:
                    target_instance_type_info = self.ast_context.lookup_type_info_by_node(node.func.value.get())
                    if self._assert_resolved_type(target_instance_type_info, "cannot resolve the instance [%s] is called on: %s" % (func_name, ast.dump(node.func.value.get()))):
                        func = self.ast_context.get_method(func_name, target_instance_type_info)
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
        append_functions = ["append", "add-to-list"]
        all_append_names = append_methods + append_functions
        assert isinstance(node, ast.AST)
        assert isinstance(func, context.Function)
        func.register_invocation(arg_type_infos)
        if func.populates_target_instance_container or func.name in all_append_names:

            # in order to understand what type containers store,
            # we need to track some special method calls
            # canonical example
            # l=[]
            # l.append(1) # <-- this tells us we have a list of ints

            if isinstance(node.func, ast.Attribute) or func.name in append_functions:
                # (shouldn't this be encapsulate in the func instance
                # somehow?)
                # we get in here if the func is the method call
                # l.append - but this may not true anymore
                # after ast rewrites have happened - see below at ***
                # node.func.value: Call.func is an Attribute instance
                # func.value -> the instance that has the attr value

                # TODO rename target_instance_type_info 
                assert len(arg_type_infos) > 0
                if func.name in append_functions and func.target_instance_type_info is None:
                    # has to be part of func md
                    target_instance_type_info = arg_type_infos[0]
                    el_ti = arg_type_infos[1]
                else:
                    target_instance_type_info = self.ast_context.lookup_type_info_by_node(node.func.value)
                    el_ti = arg_type_infos[0]
                if self._assert_resolved_type(target_instance_type_info, "Cannot lookup container type info %s" % node.func):
                    target_instance_type_info.register_contained_type(0, el_ti)
                    if func.name == "put":
                        # func md!
                        val_ti = arg_type_infos[1]
                        target_instance_type_info.register_contained_type(1, val_ti)
                    
                    #target_inst_name = node.func.value.id
                    #decl_node = self._get_declaration_node_for_ident_name(target_inst_name)
                    # if not nodeattrs.has_type_info(decl_node):
                    #     # *** since we don't get here again,
                    #     # potentially, once ast rewrites have happened,
                    #     # we need to permanently store this type info
                    #     # on the node
                    #     pass
                    #     #nodeattrs.set_type_info(decl_node, target_instance_type_info)

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
            # problematic because can register diff return types depending
            # on call site - list.get returns diff values for ex
            #func.register_rtn_type(rtn_type_info) -
        else:
            rtn_type_info = func.get_rtn_type_info()
        # if func.name == "append" and func.target_instance_type_info is None:
        #     # needs to be part of func md
        #     rtn_type_info = target_instance_type_info # container_ti
        # if func.name in all_append_names: !!!!!!
        #     rtn_type_info = context.TypeInfo.none()
        if self._assert_resolved_type(rtn_type_info, "no rtn type for func %s %s" % (func.name, node.func)):
            if rtn_type_info.has_late_resolver:
                rtn_type_info = rtn_type_info.apply_late_resolver(arg_type_infos[0])
            self.ast_context.register_type_info_by_node(node, rtn_type_info)

        # for other visitors, for example CallsiteVisitor
        nodeattrs.set_function(node, func)

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
        func.has_definition = True
        # needed for PointerVisitor
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
                    func.clear_registered_arg_type_infos()
                    for i, caller_arg_type_info in enumerate(invocation):
                        arg_node = node.args.args[i].get()
                        funcdef_arg_type_info = caller_arg_type_info
                        if nodeattrs.get_attr(arg_node, nodeattrs.IS_POINTER_NODE_ATTR):
                            funcdef_arg_type_info = self._ensure_pointer_ti(funcdef_arg_type_info)
                        self._register_type_info_by_node(arg_node, funcdef_arg_type_info)
                        func.register_arg_type_info(funcdef_arg_type_info)
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
            if self._assert_resolved_type(rtn_type_info, "cannot lookup rtn type info by node type %s" % node.value):
                if nodeattrs.get_attr(node, nodeattrs.IS_POINTER_NODE_ATTR):
                    rtn_type_info = self._ensure_pointer_ti(rtn_type_info)
                func.register_rtn_type(rtn_type_info)
                func.returns_literal = not isinstance(node.value, ast.Name)
                self._register_type_info_by_node(node, rtn_type_info)

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
            type_info = self.ast_context.lookup_type_info_by_node(node.iter.get())
            self._assert_resolved_type(type_info, "cannot lookup for loop target type by iter type %s" % ast.dump(node.iter.get()))
            if type_info is not None:
                # some of this needs to also happen in the astrewriter
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
            # why do we need to assign the type on the name node that
            # is the func name?
            func = None#nodeattrs.get_function(node, must_exist=False)
            if func is None:
                func_name = node.id
                func = self.ast_context.get_function(func_name)
            func_rtn_type_info = func.get_rtn_type_info()
            # if self._assert_resolved_type(func_rtn_type_info, "cannot get rtn type info from %s" % func):            
            #   self._register_type_info_by_node(node, func_rtn_type_info)
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
            # getting the type info from the node first is required for
            # nodes that are created by the astrewriter - they will have an
            # attached type info - for example
            #   .prepend_arg(rw.unresolved_ident(
            #       "identity", nodeattrs.QUOTE_NODE_ATTR)))
            #   the name ident created by: reassign_to_arg
            type_info = nodeattrs.get_type_info(node)
            if type_info is None:
                type_info = self._lookup_type_info_by_ident_name(node.id)
            if self._assert_resolved_type(type_info, "cannot find type info for ident '%s'" % node.id):
                self._register_type_info_by_node(node, type_info)

    def constant(self, node, num_children_visited):
        super().constant(node, num_children_visited)
        #if not self.ast_context.has_type_info(node):
        self._register_literal_type(node, node.value)

    def expr(self, node, num_children_visited):
        super().expr(node, num_children_visited)
        if num_children_visited == -1:
            value_node = node.value.get()
            type_info = self.ast_context.lookup_type_info_by_node(value_node)
            if self._assert_resolved_type(type_info, "cannot find type info for expr value node '%s'" % ast.dump(value_node)):            
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
        for ident_name in scope.get_identifiers_in_this_scope():
            decl_node = self._get_declaration_node_for_ident_name(ident_name, scope)
            decl_type_info = self.ast_context.lookup_type_info_by_node(decl_node)
            if nodeattrs.has_type_info(decl_node):
                if decl_type_info is None:
                    decl_type_info = nodeattrs.get_type_info(decl_node)
                    # this is a bit weird and covers an edge case with elisp:
                    # (dolist (s l): here s is the decl node, but it was
                    # re-rewritten as a call node. it was property registered
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
                    return True
                return False            


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
            decl_type_info = self.ast_context.get_type_info_by_node(decl_node)
            for ident_node in scope.get_identifier_nodes_in_this_scope(ident_name):
                if decl_node is ident_node:
                    continue

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

    def _lookup_type_info_by_ident_name(self, ident_name, scope=None):
        declaration_node = self._get_declaration_node_for_ident_name(ident_name, scope)
        return self.ast_context.lookup_type_info_by_node(declaration_node)

    def _assert_resolved_type(self, type_thing, msg):
        if not isinstance(type_thing, (list, tuple)):
            # multiple types can optionally be passed in
            type_thing = [type_thing]
        # when all types have been determined, type_thing should not be None
        for t in type_thing:
            if t is None:
                # print("DEBUG %s" % msg)
                self.resolved_all_type_references = False
                break
            elif isinstance(t, context.TypeInfo):
                if t.is_container:
                    for ct in t.get_contained_type_infos():
                        # not sure this is really useful, we don't know
                        # what contained type infos have been registered at this
                        # point - additionally, None types should never be
                        # registered in the firt place - finally, this needs
                        # to recurse
                        assert ct is not None, "why is there a None contained type?"
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
        type_info = context.TypeInfo(type(value))
        self._register_type_info_by_node(node, type_info)
        return type_info
