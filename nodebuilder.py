"""
Convenience methods that assemble AST bonsais.
"""


import ast
import nodes


def constant(constant_value):
    if isinstance(constant_value, ast.Constant):
        n = constant_value
    else:
        n = ast.Constant()
        n.value = constant_value
    return n


def identifier(identifier_name):
    if isinstance(identifier_name, ast.Name):
        n = identifier_name
    else:
        n = ast.Name()
        n.id = identifier_name
    return n


def assignment(lhs, rhs):
    """
    Creates a ast.Assign node with the specified lhs and rhs nodes.

    The lhs can be a str or an ast.Name node.
    """
    n = ast.Assign()
    if isinstance(lhs, ast.AST):    
        n.targets = [lhs]
    else:
        n.targets = [identifier(lhs)]
    if isinstance(rhs, ast.AST):
        n.value = rhs
    else:
        n.value = constant(rhs)
    return n


def reassignment(name, value, op=None):
    """
    Creates a ast.AugAssign node with the specified name, op and value.

    For example: a += 1

    name is a str or an ast.Name node.
    value is a Python primitive, an ast.Constant or an ast.UnaryOp
    op is a str ("+", "-" etc) - if value is a UnaryOp (-1), then op is not
    required.

    """
    n = ast.AugAssign()
    n.target = identifier(name)
    n.op = _op(op)
    if not isinstance(value, ast.AST):
        value = constant(value)
    n.value = value
    return n


def binop(operator, left, right):
    binop = ast.BinOp()
    binop.op = _op(operator)
    if not isinstance(left, ast.AST):
        left = constant(left)
    if not isinstance(right, ast.AST):
        right = constant(right)
    binop.left = left
    binop.right = right
    return binop


def _op(operator):
    assert isinstance(operator, str), "expected str but got %s" % operator
    binop = ast.BinOp()
    if operator == "+":
        return ast.Add()
    elif operator == "-":
        return ast.Sub()
    elif operator == "*":
        return ast.Mult()
    elif operator == "/":
        return ast.Div()
    else:
        assert False, "unexpected operator %s" % operator


def _op_based_on_unary(node):
    assert isinstance(node, UnaryOp)


def condition(lhs, op, rhs):
    """
    Creates and returns a ast.Compare node.
    """
    if isinstance(op, str):
        if op == "==":
            op = ast.Eq()
        elif op == "<":
            op = ast.Lt()
        elif op == ">":
            op = ast.Gt()
        else:
            assert False
    n = ast.Compare()
    n.left = identifier(lhs)
    n.ops = [op]
    n.comparators = [rhs]
    return n


def call(func, args=[], node_attrs=[], keyword=False):
    """
    Creates and returns a ast.Call node.

    func is either the function name, specified as a str, or another
    ast.AST Node, to use as the value of ast.Call.func.

    args may be a list of simple types (strings, ints etc) or ast.AST nodes.

    node_attrs is optinal node metadata set on the node instance using setattr.
    """
    n = nodes.CallAsKeyword() if keyword else ast.Call()
    if isinstance(func, str):
        n.func = identifier(func)
    else:
        n.func = func
    n.args = [a if isinstance(a, ast.AST) else constant(a) for a in args]
    n.keywords = []
    for attr in node_attrs:
        setattr(n, attr, True)
    return n


def attr_call(target, method_name, args=[], node_attrs=[]):
    """
    Creates a ast.Call node for an attribute method method call.

    target: the lhs, which is followed by '.'
    """
    assert isinstance(target, ast.AST)
    attr_node = ast.Attribute()
    attr_node.value = target
    attr_node.attr = method_name
    return call(attr_node, args, node_attrs)


def subscript_list(target, index):
    if isinstance(target, str):
        target = identifier(target)
    if isinstance(index, int):
        index = constant(index)
    elif isinstance(index, str):
        index = identifier(index)
    n = ast.Subscript()
    n.value = target
    n.slice = index
    return n


def tuple(*elts):
    n = ast.Tuple()
    n.elts = [e if isinstance(e, ast.AST) else identifier(e) for e in elts]
    return n


def if_stmt(test, body, orelse=[]):
    n = ast.If()
    n.test = test
    if isinstance(body, ast.AST):
        body = [body]
    n.body = body
    if isinstance(orelse, ast.AST):
        orelse = [orelse]
    n.orelse = orelse
    return n


def insert_node_above(insert_node, body, body_node):
    i = get_body_insert_index(body, body_node)
    body.insert(i, insert_node)


def insert_node_below(insert_node, body, body_node):
    i = get_body_insert_index(body, body_node)
    body.insert(i+1, insert_node)


def get_body_insert_index(body, node):
    for i, n in enumerate(body):
        n = n.get()
        if n is node:
            return i
        if isinstance(n, ast.Assign):
            if n.targets[0] is node:
                return i
            if n.value is node:
                return i
        if isinstance(n, ast.Expr):
            if n.value is node:
                return i
    raise Exception("Cannot find node %s in body" % node)
