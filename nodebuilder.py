"""
Convenience methods that assemble AST nodes.
"""


import ast


def constant(constant_value):
    n = ast.Constant()
    n.value = constant_value
    return n


def identifier(identifier_name):
    n = ast.Name()
    n.id = identifier_name
    return n


def call(func, args=[], node_attrs=[]):
    """
    Creates and returns a ast.Call node.

    func is either the function name, specified as a str, or another
    ast.AST Node, to use as the value of ast.Call.func.

    args may be a list of simple types (strings, ints etc) or ast.AST nodes.

    node_attrs is optinal node metadata set on the node instance using setattr.
    """
    n = ast.Call()
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


def constant_assignment(identifier_name, constant_value):
    n = ast.Assign()
    n.targets = [identifier(identifier_name)]
    n.value = constant(constant_value)
    return n


def binop(operator, left, right):
    assert isinstance(operator, str)
    binop = ast.BinOp()
    if operator == "+":
        binop.op = ast.Add()
    elif operator == "-":
        binop.op = ast.Sub()
    elif operator == "*":
        binop.op = ast.Mult()
    elif operator == "/":
        binop.op = ast.Div()
    else:
        assert False, "unexpected operator %s" % operator
    if not isinstance(left, ast.AST):
        left = constant(left)
    if not isinstance(right, ast.AST):
        right = constant(right)
    binop.left = left
    binop.right = right
    return binop
