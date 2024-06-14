import ast
import types

from target import targetlanguage
from target import targets
from visitor import visitor
import astpath
import astrewriter
import context
import copy
import nodeattrs
import nodebuilder
import nodes
import scope as scopem


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

    def loop_for(self, node, num_children_visited, is_foreach):
        super().loop_for(node, num_children_visited, is_foreach)
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


class BodyParentNodeVisitor(visitor.NoopNodeVisitor):

    def __init__(self):
        super().__init__()
        self.parent_body_stack = []

    @property
    def parent_body(self):
        return self.parent_body_stack[-1]

    def block(self, node, num_children_visited, is_root_block, body):
        super().block(node, num_children_visited, is_root_block, body)
        assert isinstance(body, (list, tuple))
        if num_children_visited == 0:
            self.parent_body_stack.append(body)
        elif num_children_visited == -1:
            self.parent_body_stack.pop()


class ContainerTypeVisitor(visitor.NoopNodeVisitor):

    def __init__(self, ast_context):
        super().__init__()
        self.ast_context = ast_context

    def assign(self, node, num_children_visited):
        super().assign(node, num_children_visited)
        if num_children_visited == -1:
            assert len(node.targets) == 1
            lhs = node.targets[0].get() # get() because array access
            if isinstance(lhs, ast.Subscript):
                # dict assignment
                self._set_container_md(node, context.DictContainerMetadata())

    def call(self, node, num_children_visited):
        super().call(node, num_children_visited)
        if num_children_visited == -1:
            if isinstance(node.func, ast.Attribute) and node.func.attr == "append":
                # we don't have types, in the end maybe this needs
                # to move into type visitor? do we get anything from having
                # this code here?

                # append to list (?)
                self._set_container_md(node, context.ListContainerMetadata())

    def _set_container_md(self, node, md):
        if not nodeattrs.has_container_md(node):
            nodeattrs.set_container_md(node, md)

class FuncCallVisitor(_CommonStateVisitor, BodyParentNodeVisitor):
    """
    Executes rewrite rules on the AST - adds new nodes that are then
    visited instead of the previous nodes.
    """
    def __init__(self, ast_context, target):
        super().__init__(ast_context, target)
        self._keep_revisiting = False

    @property
    def leave_early(self):
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
                    self._handle_function_call("<>_=", None, node, arg_nodes=[lhs.get(), node.value])

    def assign_aug(self, node, num_children_visited):
        super().assign_aug(node, num_children_visited)
        if num_children_visited == -1:
            op = self._get_op(node)
            n = "<>_=_aug_%s" % op
            self._handle_function_call(n, None, node, arg_nodes=[node.target, node.value])

    def unaryop(self, node, num_children_visited):
        super().unaryop(node, num_children_visited)
        if num_children_visited == -1:
            op = self._get_op(node)
            self._handle_function_call("<>_unary%s" % op, None, node, [node.operand])

    def binop(self, node, num_children_visited):
        super().binop(node, num_children_visited)
        if num_children_visited == -1:
            op = self._get_op(node)
            self._handle_function_call("<>_%s" % op, None, node, [node.left, node.right])

    def _get_op(self, node):
        if isinstance(node.op, (ast.Add, ast.UAdd)):
            op = "+"
        elif isinstance(node.op, (ast.Sub, ast.USub)):
            op = "-"
        elif isinstance(node.op, ast.Div):
            op = "/"
        elif isinstance(node.op, ast.Mult):
            op = "*"
        elif isinstance(node.op, ast.Mod):
            op = "%"
        elif isinstance(node.op, ast.Not):
            op = "not"
        else:
            assert False, "Unhandled op %s" % node.op
        return op

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
            elif isinstance(node.ops[0], ast.NotEq):
                op = "<>_!="                
            elif isinstance(node.ops[0], ast.Is):
                op = "<>_is"
            elif isinstance(node.ops[0], ast.IsNot):
                op = "<>_is_not"
            elif isinstance(node.ops[0], ast.Lt):
                op = "<>_less_than"
            elif isinstance(node.ops[0], ast.Gt):
                op = "<>_greater_than"
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

    def cond_if(self, node, num_children_visited):
        super().cond_if(node, num_children_visited)
        if num_children_visited == -1:
            self._handle_function_call("<>_if", None, node, arg_nodes=[node.test])

    def cond_if_expr(self, node, num_children_visited):
        super().cond_if_expr(node, num_children_visited)
        if num_children_visited == -1:
            self._handle_function_call("<>_if_expr", None, node, arg_nodes=[node.test])

    def loop_for(self, node, num_children_visited, is_foreach):
        super().loop_for(node, num_children_visited, is_foreach)
        if num_children_visited == -1:
            self._handle_function_call("<>_loop_for", None, node, arg_nodes=[node.target, node.iter])

    def subscript(self, node, num_children_visited):
        super().subscript(node, num_children_visited)
        if num_children_visited == -1:
            target_node = node.value
            target_type = self.ast_context.get_type_info_by_node(target_node).value_type
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
            target_type_info = self.ast_context.get_type_info_by_node(target_node)
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
                                         self.parent_body,
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


class IfExprRewriter(visitor.NoopNodeVisitor):
    """
    Rewrites a Python style if-expression as a regular, plain old if-statement.
    """
    def __init__(self, ast_context):
        super().__init__()
        self.ast_context = ast_context

    def assign(self, node, num_children_visited):
        super().assign(node, num_children_visited)
        if num_children_visited == -1:
            if isinstance(node.value, ast.IfExp):
                self._handle(if_exp_node=node.value,
                             if_exp_parent_node=node)

    def call(self, node, num_children_visited):
        super().call(node, num_children_visited)
        if num_children_visited == -1:
            for arg in node.args:
                if isinstance(arg, ast.IfExp):
                    self._handle(if_exp_node=arg,
                                 if_exp_parent_node=node)

    def rtn(self, node, num_children_visited):
        super().rtn(node, num_children_visited)
        if num_children_visited == -1:
            if isinstance(node.value, ast.IfExp):
                self._handle(if_exp_node=node.value,
                             if_exp_parent_node=node)

    def lambdadef(self, node, num_children_visited):
        super().rtn(node, num_children_visited)
        if num_children_visited == -1:
            if isinstance(node.body, ast.IfExp):
                self._handle(if_exp_node=node.body,
                             if_exp_parent_node=node)

    def _handle(self, if_exp_node, if_exp_parent_node):
        # a = 3 if 0 == 0 else 2
        # body: 3 <Constant Node>
        # test: 0 == 0 <Compare Node>
        # orelse: 2 <Constant Node>
        # if_expr_parent_node: a = <IfExp Node>
        arg_nodes=(if_exp_node.body,
                   if_exp_node.test,
                   if_exp_node.orelse,
                   if_exp_parent_node)
                    
        rw = astrewriter.ASTRewriter(if_exp_node,
                                     arg_nodes,
                                     self.ast_context,
                                     parent_body=None)
        rw.rewrite_as_if_stmt()


class IfExprToTernaryRewriter(visitor.NoopNodeVisitor):
    """
    Rewrites a Python style if-expression as  a c-style ternary if-expression.

    This rewriter needs to run last, towards the end, as it breaks the
    TypeVisitor. It is really more of a syntax manipulation than an ast rewrite,
    but implementing it as an ast rewrite makes this logic simple.
    """
    def cond_if_expr(self, node, num_children_visited):
        """
        a = 3 if 0 == 0 else 2
        3: node.body
        0 == 0: node.test
        2: node.orelse
        """
        super().cond_if_expr(node, num_children_visited)
        if num_children_visited == -1:
            body = node.body
            test = node.test
            node.body = test
            node.test = body


class BlockScopePuller(_CommonStateVisitor):
    """
    Pulls declaration made in a block and referenced outside out of the block.

    if 1 == 1:
        name = "water"
    print(name)

    -> 

    name = None
    if 1 == 1:
        name = "water"
    print(name)
    """

    def __init__(self, ast_context, target):
        super().__init__(ast_context, target)
        # tracks the ident that requires a declaration in a parent scope
        self.ident_name_to_scope = {}

    def name(self, node, num_children_visited):
        super().name(node, num_children_visited)
        if num_children_visited == 0:
            if not self.visiting_func:
                scope = self.ast_context.current_scope.get()
                if not scope.is_declaration_node(node):
                    if not scope.has_been_declared(node.id):
                        self.ident_name_to_scope[node.id] = scope

    def on_scope_released(self, scope):
        if scope.has_namespace or not scope.has_parent:
            for ident_name, scope in self.ident_name_to_scope.items():
                decl_node = nodebuilder.assignment(ident_name, None)
                declaring_scope = scope.get_declaring_child_scopes(ident_name)[0]
                nodes.insert_node_above(decl_node, scope.ast_node.body,
                                        declaring_scope.ast_node)
            self.ident_name_to_scope = {}


class PointerVisitor(visitor.NoopNodeVisitor):

    def __init__(self, ast_context):
        super().__init__()
        self.ast_context = ast_context

        # - these should move out to the target language definition
        # - since tuple is read-only, it doesn't need to be passed by reference?
        #   but null checks won't work
        # - review this actually - making things pointers just for null checks
        # is probably dumb
        self.pass_by_reference_types = (list, str,)

    def funcdef(self, node, num_children_visited):
        """
        Iterates over user defined functions and changes reference types to
        pointers.
        """
        super().funcdef(node, num_children_visited)
        if num_children_visited == -1:
            func = nodeattrs.get_function(node)
            for i, arg_ti in enumerate(func.arg_type_infos):
                if arg_ti.value_type in self.pass_by_reference_types:
                    arg_node = node.args.args[i].get()
                    nodeattrs.set_attr(arg_node, nodeattrs.IS_POINTER_NODE_ATTR)

    def rtn(self, node, num_children_visited):
        super().rtn(node, num_children_visited)
        if num_children_visited == -1:
            ti = self.ast_context.get_type_info_by_node(node)
            if ti.value_type in self.pass_by_reference_types:
                nodeattrs.set_attr(node, nodeattrs.IS_POINTER_NODE_ATTR)


class PointerHandlerVisitor(BodyParentNodeVisitor):
    """
    Adds pointer dereference (*) and address of (&) operators.
    """

    def __init__(self, ast_context):
        super().__init__()
        self.ast_context = ast_context

    def call(self, node, num_children_visited):
        """
        Processes callsites of user defined functions and makes sure that
        pointers are passed when required.
        """
        super().call(node, num_children_visited)
        if num_children_visited == -1:
            func = nodeattrs.get_function(node, must_exist=True)
            if func.is_builtin:
                # THIS IS OBVIOUSLY WRONG BUT FOR NOW:
                # we assume builtins don't want pointers
                first_arg_ti = None
                for i, n in enumerate(node.args):
                    n = n.get()
                    ti = self.ast_context.get_type_info_by_node(n)
                    if i == 0:
                        first_arg_ti = ti
                    if ti.is_pointer:
                        if func.name == "append" and i == 1:
                            # FIXME
                            # can this use late_resolver, set at rewrite?
                            if first_arg_ti.get_contained_type_info_at(0).is_pointer:
                                # append(l, foo), l has pointers, foo is one,
                                # don't do anything
                                pass
                        else:
                            nodeattrs.set_attr(n, nodeattrs.DEREF_NODE_MD)
            else:
                assert len(func.arg_type_infos) == len(node.args)
                for i, arg_ti in enumerate(func.arg_type_infos):
                    call_arg_node = node.args[i].get()
                    call_ti = self.ast_context.get_type_info_by_node(call_arg_node)
                    self._handle_pointer(arg_ti, call_ti, call_arg_node)

            # check if we are trying to take the address of any literals
            for i, arg_node in enumerate(node.args):
                arg_node = arg_node.get()
                if nodeattrs.get_attr(arg_node, nodeattrs.ADDRESS_OF_NODE_MD):
                    if not isinstance(arg_node, ast.Name):
                        ident_node = self._add_assignment_to_tmp_ident(node, arg_node)
                        nodeattrs.set_attr(ident_node, nodeattrs.ADDRESS_OF_NODE_MD)
                        node.args[i] = ident_node

    def compare(self, node, num_children_visited):
        super().compare(node, num_children_visited)
        if num_children_visited == -1:
            # incomplete ... - just getting a test to pass for now:
            # if a == "foo" -> if *a == "foo"
            if isinstance(node.left, ast.Name):
                left_ti = self.ast_context.get_type_info_by_node(node.left)
                rhs = node.comparators[0]
                if isinstance(rhs, ast.Constant):
                    if left_ti.is_pointer:
                        rhs_ti = self.ast_context.get_type_info_by_node(rhs)
                        if not rhs_ti.is_none_type:
                            nodeattrs.set_attr(node.left, nodeattrs.DEREF_NODE_MD)

    def rtn(self, node, num_children_visited):
        super().rtn(node, num_children_visited)
        if num_children_visited == -1:
            rtn_type_info = self.ast_context.get_type_info_by_node(node)
            returned_node = node.value
            ti = self.ast_context.get_type_info_by_node(returned_node)
            self._handle_pointer(rtn_type_info, ti, returned_node)
            if nodeattrs.get_attr(returned_node, nodeattrs.ADDRESS_OF_NODE_MD):
                if not isinstance(returned_node, ast.Name):
                    # we need a name node to deref or take the address
                    # of - but this isn't specific to rtn?
                    # return "foo"
                    # ->
                    # a = "foo"
                    # return &a
                    # (this works in golang, won't work for c ...)
                    if (isinstance(returned_node, ast.Constant) and
                        returned_node.value is None):
                        # special case - return nil is fine
                        pass
                    else:
                        ident_node = self._add_assignment_to_tmp_ident(node, node.value)
                        node.value = ident_node
                        self._handle_pointer(rtn_type_info, ti, node.value)

    def assign(self, node, num_children_visited):
        super().assign(node, num_children_visited)
        if num_children_visited == -1:
            scope = self.ast_context.current_scope.get()
            lhs = node.targets[0].get()
            lhs_ti = self.ast_context.get_type_info_by_node(lhs)
            rhs = node.value
            if isinstance(rhs, ast.Call):
                func = nodeattrs.get_function(rhs)
                # expose is_builtin (-> not is custom func)
                if func.is_builtin:
                    # this is a builtin function - for now we assume
                    # builtins don't return pointers - this is obviously
                    # wrong
                    if lhs_ti.is_pointer:
                        nodeattrs.set_attr(lhs, nodeattrs.DEREF_NODE_MD)
                else:
                    rtn_ti = func.get_rtn_type_info()
                    if lhs_ti != rtn_ti:
                        if rtn_ti.is_pointer and not lhs_ti.is_pointer:
                            # if we are changing the type to pointer, this
                            # better be a declaration node (ie not an ident
                            # being re-used)
                            assert scope.is_declaration_node(lhs)
                            lhs_ti.is_pointer = True
                        else:
                            raise AssetionError("why are these types not matching?")
            elif isinstance(rhs, ast.Subscript):
                self._handle_subscript_rhs(rhs)
                rhs_ti = self.ast_context.get_type_info_by_node(rhs)
                # why call this generic method only here?
                self._handle_pointer(lhs_ti, rhs_ti, rhs.value)
            elif isinstance(rhs, ast.Constant):
                if isinstance(lhs, ast.Name):
                    # since we are assigning a constant, we need to look at the
                    # type of the declaration node, because typevisitor just
                    # looks at the type at the rhs
                    decl_node = scope.get_declaration_node(lhs.id)
                    decl_node_ti = self.ast_context.get_type_info_by_node(decl_node)
                    if decl_node_ti.is_pointer:
                        nodeattrs.set_attr(lhs, nodeattrs.DEREF_NODE_MD)

    def subscript(self, node, num_children_visited):
        super().subscript(node, num_children_visited)
        if num_children_visited == -1:
            self._handle_subscript_rhs(node)

    def _add_assignment_to_tmp_ident(self, node, rhs_node):
        ident_name = self.ast_context.get_unique_identifier_name()
        ident_assignment = nodebuilder.assignment(ident_name, rhs_node)
        lhs_node = ident_assignment.targets[0]
        ti = self.ast_context.get_type_info_by_node(rhs_node)
        self.ast_context.register_type_info_by_node(lhs_node, ti)
        nodes.insert_node_above(ident_assignment, self.parent_body, node)
        if isinstance(rhs_node, ast.Subscript):
            self._handle_subscript_rhs(rhs_node)
        return copy.copy(lhs_node)

    def _handle_subscript_rhs(self, node):
        """
        TODO handle this generically with a visit method, add multiple passes
        to this visitor, run it in the last pass.
        """
        assert isinstance(node, ast.Subscript)
        if isinstance(node.value, ast.Name):
            #   val = l[0] # if l is a pointer, we need:
            # ->
            #   val = (*l)[0]
            ti = self.ast_context.get_type_info_by_node(node.value)
            if ti.is_pointer:
                nodeattrs.set_attr(node, nodeattrs.DEREF_NODE_MD)
                nodeattrs.set_attr(node.value, nodeattrs.DEREF_NODE_MD)
        if isinstance(node.slice, ast.Name):
            #   val = d[key] # if key is a pointer, we need
            # ->
            #   val = d[*key]
            ti = self.ast_context.get_type_info_by_node(node.slice)
            if ti.is_pointer:
                nodeattrs.set_attr(node.slice, nodeattrs.DEREF_NODE_MD)

    def _handle_pointer(self, required_type_info, type_info, node):
        if required_type_info.is_pointer:
            if type_info.is_none_type:
                # ok - pass null to something that takes a pointer
                pass
            elif not type_info.is_pointer:
                nodeattrs.set_attr(node, nodeattrs.ADDRESS_OF_NODE_MD)
        else:
            if type_info.is_pointer:
                nodeattrs.set_attr(node, nodeattrs.DEREF_NODE_MD)


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
            insert_index = nodes.get_body_insert_index(scope.ast_node.body, node) + 1
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
                        func = nodeattrs.get_function(node)
                        func.docstring = node.body[0].value.value
                        del node.body[0]


class CallsiteVisitor(visitor.NoopNodeVisitor):
    """
    This visitor collects function caller information.

    If the caller assigns to a single ident, then the function returns a tuple.
    If the caller assigns to multiple values, the function can return multiple
    values. This seems like good default behavior.
    """
    def assign(self, node, num_children_visited):
        super().assign(node, num_children_visited)
        if num_children_visited == 0:
            assert len(node.targets) == 1
            lhs = node.targets[0].get()
            rhs = node.value
            if isinstance(rhs, ast.Call):
                func = nodeattrs.get_function(rhs)
                unpacking = isinstance(lhs, ast.Tuple)
                # TODO we don't need 2 complementary booleans here?
                func.caller_unpacks_return_value = unpacking
                func.caller_assigns_single_return_value = not unpacking


class UnpackingRewriter(BodyParentNodeVisitor):
    """
    If the target language does not support unpacking a tuple
    (not has_assignment_lhs_unpacking):

    a, b = [1, 2]
    =>
    t0 = [1, 2]
    a = t0[0]
    b = t0[1]
    """
    def __init__(self, ast_context, target):
        super().__init__()
        self.ast_context = ast_context
        self.target = target

    def assign(self, node, num_children_visited):
        super().assign(node, num_children_visited)
        if num_children_visited == 0:
            assert len(node.targets) == 1
            lhs = node.targets[0].get()
            rhs = node.value
            rewrite = self._should_rewrite(lhs, rhs)
            if rewrite:
                if isinstance(rhs, ast.Name):
                    ident_node = rhs
                    # skip the original assignment node
                    setattr(node, nodeattrs.SKIP_NODE_ATTR, True)
                else:
                    ident_name = self.ast_context.get_unique_identifier_name()
                    ident_node = nodebuilder.identifier(ident_name)
                    setattr(lhs, nodeattrs.ALT_NODE_ATTR, ident_node)
                self._add_subscribt_assignments(node, ident_node, lhs.elts)

    def loop_for(self, node, num_children_visited, is_foreach):
        super().loop_for(node, num_children_visited, is_foreach)
        if num_children_visited == 0:
            rewrite = True
            if isinstance(node.iter, ast.Call):
                func = nodeattrs.get_function(node.iter)
                if func.name == "enumerate":
                    # enumerate in a for loop is handled by each target language
                    # impl - it would be nice to make this more explicit
                    # also, we should check for "builtin" here?
                    rewrite = False
            if rewrite:
                rewrite = self._should_rewrite(node.target, node.iter)
            if rewrite:
                ident_name = self.ast_context.get_unique_identifier_name()
                ident_node = nodebuilder.identifier(ident_name)
                for i, target_node in enumerate(node.target.elts):
                    n = nodebuilder.assignment(
                        target_node,
                        nodebuilder.subscript_list(ident_name, i))
                    node.body.insert(i, n)
                setattr(node.target, nodeattrs.ALT_NODE_ATTR, ident_node)

    def _add_subscribt_assignments(self, node, list_ident_node, target_nodes):
        insert_index = nodes.get_body_insert_index(self.parent_body, node) + 1
        varname = list_ident_node.id
        for i, target_node in enumerate(target_nodes):
            if target_node.id == "_":
                # by convention, "_" is the "throwaway identifier" in Python
                # for example: first_name, _ = full_name.split(" ")
                # we are skipping it when rewriting unpacking as individual
                # assignments
                continue
            n = nodebuilder.assignment(
                target_node,
                nodebuilder.subscript_list(varname, i))
            self.parent_body.insert(insert_index, n)
            insert_index += 1

    def _should_rewrite(self, lhs, rhs):
        rewrite = isinstance(lhs, ast.Tuple)
        if isinstance(rhs, ast.Call):
            func = nodeattrs.get_function(rhs)
            # this isn't right when this rewrite happens for a for-loop
            # the return type of the function is not what the iteration
            # variable actually gets - it gets the contained type instead
            if func.returns_multiple_values(self.target):
                rewrite = False
            if self.target.has_assignment_lhs_unpacking:
                rewrite = False
        else:
            if self.target.has_assignment_lhs_unpacking:
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

    def on_scope_released(self, scope):
        super().on_scope_released(scope)
        self.ident_names.update(scope.get_identifiers_in_this_scope())

    def sayonara(self):
        super().sayonara()
        self.ast_context.register_ident_names(self.ident_names)


class LambdaReturnVisitor(visitor.NoopNodeVisitor):
    """
    Adds return stmt to lambda function bodies unless "explicit_rtn" is False or
    unless this is Python. (return stmt removal is done in tokenvisitor for
    simplicity).
    """
    def __init__(self, ast_context, target):
        super().__init__()
        self.ast_context = ast_context
        self.target = target

    def lambdadef(self, node, num_children_visited):
        super().lambdadef(node, num_children_visited)
        if num_children_visited == -1:
            if self.target.explicit_rtn and not targets.is_python(self.target):
                ti = self.ast_context.get_type_info_by_node(node.body)
                if ti.is_real:
                    rtn_node = nodebuilder.rtn(nodes.shallow_copy_node(node.body, self.ast_context))
                    nodeattrs.set_attr(node.body, nodeattrs.ALT_NODE_ATTR, rtn_node)


class ReturnValueMapper(BodyParentNodeVisitor):
    """
    The difficulty about transcompiling is the potential abstraction mismatch
    between the source and the target language.
    In its simplest form, this mismatch manifests itself as different "default"
    or "marker" return values. For example the Python index method on strings
    returns -1 if a substring isn't found. In Elisp, a similar function,
    cl-search, return nil if the substring isn't found.

    There are multiple possible strategies to address this problem:
      - Wrapper functions, ie wrap cl-search with a custom function that returns
        -1 instead of nil when the substring isn't found
      - Track how the return value is used and adjust comparisions of necessary
        so look for all "i == -1" checks and change them to "i is None".
      - Right after calling the function, insert an if stmt that maps -1 to None
    
    This visitor implements the latter approach.
    """

    MAPPED_RTN_VALUE_OLD_VALUE_ATTR = "__rtn_value_mapping_old_value"
    MAPPED_RTN_VALUE_NEW_VALUE_ATTR = "__rtn_value_mapping_new_value"
    
    def __init__(self, ast_context):
        super().__init__()
        self.ast_context = ast_context

    def call(self, node, num_children_visited):
        super().call(node, num_children_visited)
        if num_children_visited == -1:
            assign_nodes = nodes.extract_expressions_with_attr(
                node, self.parent_body,
                ReturnValueMapper.MAPPED_RTN_VALUE_OLD_VALUE_ATTR,
                self.ast_context,
                tmp_ident_prefix="i") # TODO
            for n in assign_nodes:
                lhs = nodes.get_assignment_lhs(n)
                rhs = nodes.get_assignment_rhs(n)
                old_value = nodeattrs.get_attr(
                    rhs, ReturnValueMapper.MAPPED_RTN_VALUE_OLD_VALUE_ATTR,
                    remove_attr=True, must_exist=True)
                new_value = nodeattrs.get_attr(
                    rhs, ReturnValueMapper.MAPPED_RTN_VALUE_NEW_VALUE_ATTR,
                    remove_attr=True, must_exist=True)
                if_stmt = nodebuilder.if_stmt(
                    nodebuilder.compare(lhs.id, "==", nodebuilder.constant(old_value)),
                    body=nodebuilder.assignment(lhs.id, nodebuilder.constant(new_value)))
                nodes.insert_node_below(
                    if_stmt, self.parent_body, n)


class LameSemanticCheckerVisitor(_CommonStateVisitor):
    """
    Detects simple errors in the AST.
    """
    def name(self, node, num_children_visited):
        super().name(node, num_children_visited)
        if self.visiting_func:
            pass
        elif self.assign_visiting_lhs:
            pass
        elif self.loop_visiting_lhs:
            pass
        else:
            scope = self.ast_context.current_scope.get()
            decl_node = scope.get_declaration_node(node.id)
            assert decl_node is not None, "Unknown identifier [%s]" % node.id


class NodeCollectingVisitor(visitor.NoopNodeVisitor):
    
    def __init__(self, condition_callback):
        super().__init__()
        self.condition_callback = condition_callback
        self.nodes = []

    def generic_visit(self, node, num_children_visited):
        super().generic_visit(node, num_children_visited)
        if num_children_visited == -1:
            if self.condition_callback(node):
                self.nodes.append(node)
