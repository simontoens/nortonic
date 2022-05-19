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


def call(function_name):
    n = ast.Call()
    n.func = identifier(function_name)
    n.args = []
    n.keywords = []
    return n


def constant_assignment(identifier_name, constant_value):
    n = ast.Assign()
    n.targets = [identifier(identifier_name)]
    n.value = constant(constant_value)
    return n



