import ast
import types

from target import targetlanguage
from visitor import visitor
import astpath
import astrewriter
import context
import copy
import nodeattrs
import nodebuilder


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

    def assign_aug(self, node, num_children_visited):
        super().assign_aug(node, num_children_visited)
        if num_children_visited == -1:
            op = self._get_op(node)
            n = "<>_=_aug_%s" % op
            self._handle_function_call(n, None, node, arg_nodes=[node.target.get(), node.value.get()])


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
        else:
            assert False, "Unhandled binop %s" % node.op
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
            elif isinstance(node.ops[0], ast.Is):
                op = "<>_is"
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


class IfExprRewriter(visitor.NoopNodeVisitor):

    def cond_if_expr(self, node, num_children_visited):
        """
        a = 3 if 0 == 0 else 2
        3: node.body
        0 == 0: node.test
        2: node.orelse
        """
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

    def name(self, node, num_children_visited):
        super().name(node, num_children_visited)
        if num_children_visited == 0:
            if not self.visiting_func:
                scope = self.ast_context.current_scope.get()
                if not scope.is_declaration_node(node):
                    if not scope.has_been_declared(node.id):
                        n = nodebuilder.assignment(node.id, None)
                        scope.ast_node.body.insert(0, n)


class PointerVisitor(_CommonStateVisitor):

    def __init__(self, ast_context, target):
        super().__init__(ast_context, target)
        self.num_visits = 0

    @property
    def should_revisit(self):
        self.num_visits += 1        
        return self.num_visits < 2

    def funcdef(self, node, num_children_visited):
        # processed on initial visit
        if self.num_visits == 0:
            super().funcdef(node, num_children_visited)
            if num_children_visited == -1:
                for arg_node in node.args.args:
                    arg_node = arg_node.get()
                    ti = self.ast_context.lookup_type_info_by_node(arg_node)
                    if ti.value_type in (list, tuple, dict): # hack hack hack
                        ti.is_pointer = True

    def call(self, node, num_children_visited):
        # processed on 2nd visit
        if self.num_visits == 1:
            func_name = super().call(node, num_children_visited)
            if func_name == "range":
                # hack hack - range is going away anyway, args have no type inf
                return
            if num_children_visited == -1:
                # experiment a bit yeah - we need a proper way to get the
                # Function instance
                pass_by_value = True if func_name in ("fmt.Println", "append") else False
                for arg_node in node.args:
                    arg_node = arg_node.get()
                    if isinstance(arg_node, ast.Name):
                        ti = self.ast_context.lookup_type_info_by_node(arg_node)
                        if ti.is_pointer and pass_by_value:
                            arg_node.get_node_metadata()["deref"] =  True
                        elif not ti.is_pointer and not pass_by_value:
                            arg_node.get_node_metadata()["address"] =  True

    def assign(self, node, num_children_visited):
        """
        Handle this:
        func f1(l *[]int) {
            fmt.Println(*l)
            l = append(l, 4)  <--- 
}
        """
        # processed on 2nd visit
        if self.num_visits == 1:
            super().assign(node, num_children_visited)
            if num_children_visited == -1:
                lhs = node.targets[0].get()
                rhs = node.value.get()
                rtn_by_value = True
                if isinstance(rhs, ast.Call):
                    lhs_ti = self.ast_context.lookup_type_info_by_node(lhs)
                    if lhs_ti.is_pointer and rtn_by_value:
                        lhs.get_node_metadata()["deref"] =  True
                    elif not lhs_ti.is_pointer and not rtn_by_value:
                        # TODO function that returns a pointer (to a list)
                        # somethung like:
                        # s := "foo22"
	                # s = *f1()
                        # but this is rtn by value - the value is copied in the
                        # end
                        assert False, "the rhs returns a pointer, but the lhs is not a pointer"


    def subscript(self, node, num_children_visited):
        # processed on 2nd visit
        if self.num_visits == 1:
            super().subscript(node, num_children_visited)
            if num_children_visited == -1:
                value_node = node.value.get()
                type_info = self.ast_context.lookup_type_info_by_node(value_node)
                if type_info.is_pointer and type_info.is_sequence:
                    # dict? - use type mapping instead
                    value_node.get_node_metadata()["deref_w_paren"] =  True


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
            rhs = node.value.get()
            if isinstance(rhs, ast.Call):
                func = nodeattrs.get_function(rhs)
                unpacking = isinstance(lhs, ast.Tuple)
                func.caller_unpacks_return_value = unpacking
                func.caller_assigns_single_return_value = not unpacking


class UnpackingRewriter(_BodyParentNodeVisitor):
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

    @property
    def should_revisit(self):
        self.ast_context.register_ident_names(self.ident_names)
        return False

    def on_scope_released(self, scope):
        super().on_scope_released(scope)
        self.ident_names.update(scope.get_identifiers_in_this_scope())
