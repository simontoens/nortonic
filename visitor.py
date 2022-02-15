import ast
import nodeattrs


class NoopNodeVisitor:

    @property
    def keep_visiting(self):
        """
        This visitor instance keeps visiting the AST until the property is
        False.
        """
        return False

    def done(self):
        """
        Called when all nodes of the AST have been visited.
        """
        pass

    def add(self, node, num_children_visited):
        pass

    def attr(self, node, num_children_visited):
        pass

    def binop(self, node, num_children_visited):
        pass

    def assign(self, node, num_children_visited):
        pass

    def call(self, node, num_children_visited):
        pass

    def compare(self, node, num_children_visited):
        pass

    def cond_if(self, node, num_children_visited):
        pass

    def cond_else(self, node, num_children_visited):
        pass

    def constant(self, node, num_children_visited):
        pass

    def eq(self, node, num_children_visited):
        pass

    def expr(self, node, num_children_visited):
        pass

    def funcdef(self, node, num_children_visited):
        pass

    def lst(self, node, num_children_visited):
        pass

    def module(self, node, num_children_visited):
        pass

    def mult(self, node, num_children_visited):
        pass

    def name(self, node, num_children_visited):
        pass

    def name_constant(self, node, num_children_visited):
        pass

    def num(self, node, num_children_visited):
        pass

    def rtn(self, node, num_children_visited):
        pass

    def string(self, node, num_children_visited):
        pass


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
        if isinstance(node, ast.Add):
            visitor.add(node, 0)
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
            for i, b in enumerate(node.body):
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
        elif isinstance(node, ast.List):
            visitor.lst(node, 0)
            for i, n in enumerate(node.elts):
                _visit(n, visitor)
                visitor.lst(node, i+1)
            visitor.lst(node, -1)
        elif isinstance(node, ast.Module):
            visitor.module(node, 0)
            for i, body in enumerate(node.body):
                _visit(body, visitor)
                visitor.module(node, i+1)
        elif isinstance(node, ast.Name):
            visitor.name(node, 0)
        elif isinstance(node, ast.NameConstant):
            visitor.name_constant(node, 0)
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
