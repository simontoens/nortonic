import ast
import nodeattrs


class NoopNodeVisitor:

    def __init__(self, delegate=None):
        # if not None, visitor method calls are made on this delegate
        self._delegate = delegate

    @property
    def keep_visiting(self):
        """
        This visitor instance keeps visiting the AST until the property is
        False.
        """
        if self._delegate is None:
            return False
        else:
            return self._delegate.keep_visiting
        

    def done(self):
        """
        Called when all nodes of the AST have been visited.
        """
        if self._delegate is not None:
            self._delegate.done()

    def add(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.add(node, num_children_visited)

    def div(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.div(node, num_children_visited)
            
    def mult(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.mult(node, num_children_visited)

    def attr(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.attr(node, num_children_visited)

    def binop(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.binop(node, num_children_visited)

    def assign(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.assign(node, num_children_visited)

    def call(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.call(node, num_children_visited)

    def compare(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.compare(node, num_children_visited)

    def cond_if(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.cond_if(node, num_children_visited)

    def cond_else(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.cond_else(node, num_children_visited)

    def constant(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.constant(node, num_children_visited)

    def eq(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.eq(node, num_children_visited)

    def expr(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.expr(node, num_children_visited)

    def funcarg(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.funcarg(node, num_children_visited)

    def funcdef(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.funcdef(node, num_children_visited)

    def loop_for(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.loop_for(node, num_children_visited)

    def container_type_dict(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.container_type_dict(node, num_children_visited)
            
    def container_type_list(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.container_type_list(node, num_children_visited)

    def container_type_tuple(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.container_type_tuple(node, num_children_visited)

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

    def subscript(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.subscript(node, num_children_visited)


def visit(root, visitor):
    _visit(root, visitor)
    visitor.done()
    while visitor.keep_visiting:
        _visit(root, visitor)
        visitor.done()


def _visit(node, visitor):
    # handle special 'alt' attribute, which points to an alternative node
    # to look at instead - see astrewriter.py
    if hasattr(node, nodeattrs.ALT_NODE_ATTR):
        _visit(getattr(node, nodeattrs.ALT_NODE_ATTR), visitor)
    else:
        if isinstance(node, ast.arg):
            visitor.funcarg(node, 0)
        elif isinstance(node, ast.Add):
            visitor.add(node, 0)
        elif isinstance(node, ast.Div):
            visitor.div(node, 0)
        elif isinstance(node, ast.Mult):
            visitor.mult(node, 0)
        elif isinstance(node, ast.BinOp):
            visitor.binop(node, 0)
            _visit(node.left, visitor)
            visitor.binop(node, 1)
            _visit(node.op, visitor)
            visitor.binop(node, 2)
            _visit(node.right, visitor)
            visitor.binop(node, -1)
        elif isinstance(node, ast.Assign):
            assert len(node.targets) == 1
            visitor.assign(node, 0)
            _visit(node.targets[0], visitor)
            visitor.assign(node, 1)
            _visit(node.value, visitor)
            visitor.assign(node, -1)
        elif isinstance(node, ast.Attribute):
            visitor.attr(node, 0)
            _visit(node.value, visitor)
            # ast.Attribute also has a 'ctx' attr, which is of type ast.Load
            visitor.attr(node, -1)
        elif isinstance(node, ast.Call):
            visitor.call(node, 0)
            _visit(node.func, visitor)
            visitor.call(node, 1)
            for i, arg in enumerate(node.args):
                _visit(arg, visitor)
                visitor.call(node, i+2)
            for keyword in node.keywords:
                assert False, "keywords not handled"
            visitor.call(node, -1)
        elif isinstance(node, ast.Constant):
            visitor.constant(node, 0)
        elif isinstance(node, ast.Compare):
            visitor.compare(node, 0)
            _visit(node.left, visitor)
            assert len(node.ops) == 1
            visitor.compare(node, 1)
            _visit(node.ops[0], visitor)
            assert len(node.comparators) == 1
            visitor.compare(node, 2)
            _visit(node.comparators[0], visitor)
            visitor.compare(node, -1)
        elif isinstance(node, ast.Eq):
            visitor.eq(node, 0)
        elif isinstance(node, ast.FunctionDef):
            visitor.funcdef(node, 0)
            for a in node.args.args:
                _visit(a, visitor)
            visitor.funcdef(node, len(node.args.args) + 1)
            body = list(node.body)
            for b in body:
                _visit(b, visitor)
            visitor.funcdef(node, -1)
        elif isinstance(node, ast.If):
            visitor.cond_if(node, 0)
            _visit(node.test, visitor)
            visitor.cond_if(node, 1)
            for i, b in enumerate(node.body):
                _visit(b, visitor)
                visitor.cond_if(node, i+2)
            visitor.cond_if(node, -1)
            if len(node.orelse) > 0:
                visitor.cond_else(node, 0)
                for i, b in enumerate(node.orelse):
                    _visit(b, visitor)
                    visitor.cond_else(node, i+1)
                visitor.cond_else(node, -1)
        elif isinstance(node, ast.For):
            visitor.loop_for(node, 0)
            _visit(node.target, visitor)
            visitor.loop_for(node, 1)
            _visit(node.iter, visitor)
            visitor.loop_for(node, 2)
            body = list(node.body)
            for i, body in enumerate(body):
                _visit(body, visitor)
            visitor.loop_for(node, -1)
        elif isinstance(node, ast.Dict):
            visitor.container_type_dict(node, 0)
            assert len(node.keys) == len(node.values)
            num_children_visited = 1
            for i in range(0, len(node.keys)):
                _visit(node.keys[i], visitor)
                visitor.container_type_dict(node, num_children_visited)
                num_children_visited += 1
                _visit(node.values[i], visitor)
                visitor.container_type_dict(node, num_children_visited)
                num_children_visited += 1
            visitor.container_type_dict(node, -1)
        elif isinstance(node, ast.List):
            visitor.container_type_list(node, 0)
            for i, n in enumerate(node.elts):
                _visit(n, visitor)
                visitor.container_type_list(node, i+1)
            visitor.container_type_list(node, -1)
        elif isinstance(node, ast.Subscript):
            visitor.subscript(node, 0)
            _visit(node.value, visitor)
            visitor.subscript(node, 1)
            _visit(node.slice, visitor)
            visitor.subscript(node, -1)
        elif isinstance(node, ast.Tuple):
            visitor.container_type_tuple(node, 0)
            for i, n in enumerate(node.elts):
                _visit(n, visitor)
                visitor.container_type_tuple(node, i+1)
            visitor.container_type_tuple(node, -1)
        elif isinstance(node, ast.Module):
            visitor.module(node, 0)
            body = list(node.body)
            for i, body in enumerate(body):
                _visit(body, visitor)
                visitor.module(node, i+1)
            visitor.module(node, -1)
        elif isinstance(node, ast.Name):
            visitor.name(node, 0)
        elif isinstance(node, ast.Num):
            visitor.num(node, 0)
        elif isinstance(node, ast.Expr):
            visitor.expr(node, 0)
            _visit(node.value, visitor)
            visitor.expr(node, -1)
        elif isinstance(node, ast.Return):
            visitor.rtn(node, 0)
            _visit(node.value, visitor)
            visitor.rtn(node, -1)        
        elif isinstance(node, ast.Str):
            visitor.string(node, 0)        
        else:
            assert False, "Unknown node %s" % node
