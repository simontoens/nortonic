import ast
import nodeattrs


class NoopNodeVisitor:
    """
    Abstract base class that contains optional methods for visitors to
    implement.
    """

    def __init__(self, delegate=None):
        # if not None, visitor method calls are made on this delegate
        self._delegate = delegate


    #
    # Optional customization of re-visiting behavior
    #

    @property
    def leave_early(self):
        """
        If this property is True, visiting stops and should_revisit is called.
        """
        if self._delegate is None:
            return False
        else:
            return self._delegate.leave_early

    @property
    def should_revisit(self):
        """
        Asked for when all nodes of the AST have been visited.
        Returns True to the visitation start over, from the beginning, or
        False if once was enough.

        The default is False.
        """
        if self._delegate is None:
            return False
        else:
            return self._delegate.should_revisit


    #
    # Optional scope control callbacks
    #

    def on_scope_pushed(self, scope):
        if self._delegate is not None:
            self._delegate.on_scope_pushed(scope)

    def on_scope_released(self, scope):
        if self._delegate is not None:
            self._delegate.on_scope_released(scope)


    #
    # Visiting methods - one per AST node type
    #

    def boolop_and(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.boolop_and(node, num_children_visited)

    def boolop_or(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.boolop_or(node, num_children_visited)
        
    def add(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.add(node, num_children_visited)

    def uadd(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.uadd(node, num_children_visited)

    def sub(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.sub(node, num_children_visited)

    def usub(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.usub(node, num_children_visited)

    def div(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.div(node, num_children_visited)
            
    def mult(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.mult(node, num_children_visited)

    def mod(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.mod(node, num_children_visited)

    def attr(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.attr(node, num_children_visited)

    def unaryop(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.unaryop(node, num_children_visited)

    def binop(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.binop(node, num_children_visited)

    def boolop(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.boolop(node, num_children_visited)

    def assign(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.assign(node, num_children_visited)

    def assign_aug(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.assign_aug(node, num_children_visited)

    def call(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.call(node, num_children_visited)

    def compare(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.compare(node, num_children_visited)

    def cond_if(self, node, num_children_visited, is_expr):
        if self._delegate is not None:
            self._delegate.cond_if(node, num_children_visited, is_expr)

    def cond_else(self, node, num_children_visited, is_if_expr):
        if self._delegate is not None:
            self._delegate.cond_else(node, num_children_visited, is_if_expr)

    def constant(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.constant(node, num_children_visited)

    def eq(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.eq(node, num_children_visited)

    def less_than(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.less_than(node, num_children_visited)

    def greater_than(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.greater_than(node, num_children_visited)

    def identity(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.identity(node, num_children_visited)

    def expr(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.expr(node, num_children_visited)

    def funcarg(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.funcarg(node, num_children_visited)

    def funcdef(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.funcdef(node, num_children_visited)

    def loop_for(self, node, num_children_visited, is_foreach):
        if self._delegate is not None:
            self._delegate.loop_for(node, num_children_visited, is_foreach)

    def loop_continue(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.loop_continue(node, num_children_visited)

    def loop_break(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.loop_break(node, num_children_visited)

    def container_type_dict(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.container_type_dict(node, num_children_visited)
            
    def container_type_list(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.container_type_list(node, num_children_visited)

    def container_type_tuple(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.container_type_tuple(node, num_children_visited)

    def import_stmt(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.import_stmt(node, num_children_visited)

    def import_from_stmt(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.import_from_stmt(node, num_children_visited)

    def module(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.module(node, num_children_visited)

    def name(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.name(node, num_children_visited)

    def num(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.num(node, num_children_visited)

    def rtn(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.rtn(node, num_children_visited)

    def string(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.string(node, num_children_visited)

    def slice(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.slice(node, num_children_visited)
            
    def subscript(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.subscript(node, num_children_visited)

    def block(self, node, num_children_visited, is_root_block):
        if self._delegate is not None:
            self._delegate.block(node, num_children_visited, is_root_block)

    def stmt(self, node, num_children_visited, parent_node, is_last_body_stmt):
        if self._delegate is not None:
            self._delegate.stmt(node, num_children_visited, parent_node, is_last_body_stmt)

    def with_resource(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.with_resource(node, num_children_visited)


def visit(root, visitor, verbose=False):
    if verbose:
        print("START", visitor)
    if verbose:
        print("INITIAL VISIT", visitor)
    _visit(root, visitor, verbose)
    while visitor.should_revisit:
        if verbose:
            print("RE-VISIT", visitor)
        _visit(root, visitor, verbose)
    if verbose:
        print("END", visitor)


def _visit(node, visitor, verbose):
    if visitor.leave_early:
        return

    # handle special 'alt' attribute, which points to an alternative node
    # to look at instead - see astrewriter.py
    if hasattr(node, nodeattrs.ALT_NODE_ATTR):
        alt_node = getattr(node, nodeattrs.ALT_NODE_ATTR)
        if verbose:
            print("_visiting alt node", alt_node, "instead of", node)
        _visit(alt_node, visitor, verbose)
    elif hasattr(node, nodeattrs.SKIP_NODE_ATTR):
        pass
    else:
        if verbose:
            print("_visit node start", node)
        if isinstance(node, ast.arg):
            visitor.funcarg(node, 0)
        elif isinstance(node, ast.UnaryOp):
            visitor.unaryop(node, 0)
            _visit(node.op, visitor, verbose)
            visitor.unaryop(node, 1)
            _visit(node.operand, visitor, verbose)
            visitor.unaryop(node, -1)
        elif isinstance(node, ast.And):
            visitor.boolop_and(node, 0)
        elif isinstance(node, ast.Or):
            visitor.boolop_or(node, 0)
        elif isinstance(node, ast.Add):
            visitor.add(node, 0)
        elif isinstance(node, ast.UAdd):
            visitor.uadd(node, 0)
        elif isinstance(node, ast.Sub):
            visitor.sub(node, 0)
        elif isinstance(node, ast.USub):
            visitor.usub(node, 0)                   
        elif isinstance(node, ast.Div):
            visitor.div(node, 0)
        elif isinstance(node, ast.Mult):
            visitor.mult(node, 0)
        elif isinstance(node, ast.Mod):
            visitor.mod(node, 0)
        elif isinstance(node, ast.BinOp):
            visitor.binop(node, 0)
            _visit(node.left, visitor, verbose)
            visitor.binop(node, 1)
            _visit(node.op, visitor, verbose)
            visitor.binop(node, 2)
            _visit(node.right, visitor, verbose)
            visitor.binop(node, -1)
        elif isinstance(node, ast.BoolOp):
            visitor.boolop(node, 0)
            for i, value in enumerate(node.values):
                _visit(value, visitor, verbose)
                if i < len(node.values) - 1:
                    _visit(node.op, visitor, verbose)
            visitor.boolop(node, -1)
        elif isinstance(node, ast.Assign):
            assert len(node.targets) == 1
            visitor.assign(node, 0)
            _visit(node.targets[0], visitor, verbose)
            visitor.assign(node, 1)
            _visit(node.value, visitor, verbose)
            visitor.assign(node, -1)
        elif isinstance(node, ast.AugAssign): # a += 1
            visitor.assign_aug(node, 0)
            _visit(node.target, visitor, verbose)
            visitor.assign_aug(node, 1)
            _visit(node.op, visitor, verbose)
            visitor.assign_aug(node, 2)
            _visit(node.value, visitor, verbose)
            visitor.assign_aug(node, -1)
        elif isinstance(node, ast.Attribute):
            visitor.attr(node, 0)
            _visit(node.value, visitor, verbose)
            # ast.Attribute also has a 'ctx' attr, which is of type ast.Load
            visitor.attr(node, -1)
        elif isinstance(node, ast.Call):
            visitor.call(node, 0)
            _visit(node.func, visitor, verbose)
            visitor.call(node, 1)
            for i, arg in enumerate(node.args):
                _visit(arg, visitor, verbose)
                visitor.call(node, i+2)
            if hasattr(node, "body"):
                _visit_body_statements(node, node.body, visitor, is_root_block=False, verbose=verbose)
            for keyword in node.keywords:
                assert False, "keywords not handled"
            visitor.call(node, -1)
        elif isinstance(node, ast.Constant):
            visitor.constant(node, 0)
        elif isinstance(node, ast.Continue):
            visitor.loop_continue(node, 0)
        elif isinstance(node, ast.Break):
            visitor.loop_break(node, 0)
        elif isinstance(node, ast.Compare):
            visitor.compare(node, 0)
            _visit(node.left, visitor, verbose)
            assert len(node.ops) == 1
            visitor.compare(node, 1)
            _visit(node.ops[0], visitor, verbose)
            assert len(node.comparators) == 1
            visitor.compare(node, 2)
            _visit(node.comparators[0], visitor, verbose)
            visitor.compare(node, -1)
        elif isinstance(node, ast.Eq):
            visitor.eq(node, 0)
        elif isinstance(node, ast.Is):
            visitor.identity(node, 0)
        elif isinstance(node, ast.Lt):
            visitor.less_than(node, 0)
        elif isinstance(node, ast.Gt):
            visitor.greater_than(node, 0)
        elif isinstance(node, ast.FunctionDef):
            visitor.funcdef(node, 0)
            for a in node.args.args:
                _visit(a, visitor, verbose)
            visitor.funcdef(node, len(node.args.args) + 1)
            _visit_body_statements(node, node.body, visitor, is_root_block=False, verbose=verbose)
            visitor.funcdef(node, -1)
        elif isinstance(node, ast.If):
            visitor.cond_if(node, 0, is_expr=False)
            _visit(node.test, visitor, verbose)
            visitor.cond_if(node, 1, is_expr=False)
            _visit_body_statements(node, node.body, visitor, is_root_block=False, verbose=verbose)
            visitor.cond_if(node, -1, is_expr=False)
            if len(node.orelse) > 0:
                visitor.cond_else(node, 0, is_if_expr=False)
                _visit_body_statements(node, node.orelse, visitor, is_root_block=False, verbose=verbose)
                visitor.cond_else(node, -1, is_if_expr=False)
        elif isinstance(node, ast.IfExp):
            # traversal order matches python's syntax: 1 if 2==3 else 2
            # make body and orelse singleton lists so that the rest of
            # the code can treat them in the same way as a regular if stmt
            if isinstance(node.body, ast.AST):
                node.body = [node.body]
            if isinstance(node.orelse, ast.AST):
                node.orelse = [node.orelse]
            visitor.cond_if(node, 0, is_expr=True)
            _visit(node.body[0], visitor, verbose)
            visitor.cond_if(node, 1, is_expr=True)
            _visit(node.test, visitor, verbose)
            visitor.cond_if(node, 2, is_expr=True)
            visitor.cond_else(node, 0, is_if_expr=True)
            _visit(node.orelse[0], visitor, verbose)
            visitor.cond_else(node, -1, is_if_expr=True)
            visitor.cond_if(node, -1, is_expr=True)
        elif isinstance(node, ast.For):
            is_foreach = not hasattr(node, nodeattrs.FOR_LOOP_C_STYLE_INIT_NODE)
            visitor.loop_for(node, 0, is_foreach=is_foreach)
            if is_foreach:
                _visit(node.target, visitor, verbose)
                visitor.loop_for(node, 1, is_foreach=True)
                _visit(node.iter, visitor, verbose)
                visitor.loop_for(node, 2, is_foreach=True)
            else:
                _visit(getattr(node, nodeattrs.FOR_LOOP_C_STYLE_INIT_NODE), visitor, verbose)
                visitor.loop_for(node, 1, is_foreach=False)
                _visit(getattr(node, nodeattrs.FOR_LOOP_C_STYLE_COND_NODE), visitor, verbose)
                visitor.loop_for(node, 2, is_foreach=False)
                _visit(getattr(node, nodeattrs.FOR_LOOP_C_STYLE_EXPR_NODE), visitor, verbose)
                visitor.loop_for(node, 3, is_foreach=False)
                
            _visit_body_statements(node, node.body, visitor, is_root_block=False, verbose=verbose)
            visitor.loop_for(node, -1, is_foreach=is_foreach)
        elif isinstance(node, ast.Dict):
            visitor.container_type_dict(node, 0)
            assert len(node.keys) == len(node.values)
            num_children_visited = 1
            for i in range(0, len(node.keys)):
                _visit(node.keys[i], visitor, verbose)
                visitor.container_type_dict(node, num_children_visited)
                num_children_visited += 1
                _visit(node.values[i], visitor, verbose)
                visitor.container_type_dict(node, num_children_visited)
                num_children_visited += 1
            visitor.container_type_dict(node, -1)
        elif isinstance(node, ast.List):
            visitor.container_type_list(node, 0)
            for i, n in enumerate(node.elts):
                _visit(n, visitor, verbose)
                visitor.container_type_list(node, i+1)
            visitor.container_type_list(node, -1)
        elif isinstance(node, ast.Slice):
            assert node.step is None
            visitor.slice(node, 0)
            if node.lower is not None:
                _visit(node.lower, visitor, verbose)
            visitor.slice(node, 1)
            if node.upper is not None:
                _visit(node.upper, visitor, verbose)
            visitor.slice(node, -1)
        elif isinstance(node, ast.Subscript):
            visitor.subscript(node, 0)
            _visit(node.value, visitor, verbose)
            visitor.subscript(node, 1)
            _visit(node.slice, visitor, verbose)
            visitor.subscript(node, -1)
        elif isinstance(node, ast.Tuple):
            visitor.container_type_tuple(node, 0)
            for i, n in enumerate(node.elts):
                _visit(n, visitor, verbose)
                visitor.container_type_tuple(node, i+1)
            visitor.container_type_tuple(node, -1)
        elif isinstance(node, ast.Module):
            visitor.module(node, 0)
            _visit_body_statements(node, node.body, visitor, is_root_block=True, verbose=verbose)
            visitor.module(node, -1)
        elif isinstance(node, ast.Name):
            visitor.name(node, 0)
        elif isinstance(node, ast.Num):
            visitor.num(node, 0)
        elif isinstance(node, ast.Expr):
            visitor.expr(node, 0)
            _visit(node.value, visitor, verbose)
            visitor.expr(node, -1)
        elif isinstance(node, ast.Return):
            visitor.rtn(node, 0)
            _visit(node.value, visitor, verbose)
            visitor.rtn(node, -1)        
        elif isinstance(node, ast.Str):
            visitor.string(node, 0)
        elif isinstance(node, ast.Import):
            visitor.import_stmt(node, 0)
        elif isinstance(node, ast.ImportFrom):
            visitor.import_from_stmt(node, 0)
        elif isinstance(node, ast.With):
            visitor.with_resource(node, 0)
        else:
            assert False, "Unknown node %s" % node


def _visit_body_statements(node, body, visitor, is_root_block, verbose):
    body = list(body)
    visitor.block(node, 0, is_root_block)
    for i, child_node in enumerate(body):
        child_node = getattr(child_node, nodeattrs.ALT_NODE_ATTR, child_node)
        is_last_body_stmt = i == len(body) - 1
        visitor.stmt(child_node, 0, node, is_last_body_stmt)
        _visit(child_node, visitor, verbose)
        visitor.stmt(child_node, -1, node, is_last_body_stmt)
    visitor.block(node, -1, is_root_block)


def nstr(node):
    if isinstance(node, ast.Assign):
        return "[assign %s]" % node.targets[0].id
    if isinstance(node, ast.Attribute):
        return "[attr %s]" % node.attr
    elif isinstance(node, ast.Call):
        if isinstance(node.func, ast.Name):
            func_info = node.func.id
        elif isinstance(node.func, ast.Attribute):
            func_info = "attr %s" % node.func.attr
        else:
            func_info = "???"
        return "[call %s]" % func_info
    elif isinstance(node, ast.Expr):
        return "[expr %s]" % nstr(node.value)
    elif isinstance(node, ast.Name):
        return "[name %s]" % node.id
    else:
        return "[unknown node %s]" % node
