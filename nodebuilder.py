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


def constant_assignment(identifier_name, constant_value):
    n = ast.Assign()
    n.targets = [identifier(identifier_name)]
    n.value = constant(constant_value)
    return n



