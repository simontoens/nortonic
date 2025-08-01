"""
Factory functions that assemble AST bonsais.
"""
import ast


def constant(constant_value: str | ast.Constant) -> ast.Constant:
    if isinstance(constant_value, ast.Constant):
        n = constant_value
    else:
        n = ast.Constant()
        n.value = constant_value
    return n


def list(*elts):
    n = ast.List()
    n.elts = [e if isinstance(e, ast.AST) else constant(e) for e in elts]
    return n


def tuple(*elts):
    n = ast.Tuple()
    n.elts = [e if isinstance(e, ast.AST) else identifier(e) for e in elts]
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


def unary_not(operand):
    uop = ast.UnaryOp()
    uop.op = ast.Not()
    uop.operand = operand
    return uop


def compare(lhs, op, rhs):
    """
    Creates and returns a ast.Compare node.
    """
    if not isinstance(lhs, ast.AST):
        lhs = identifier(lhs)
    if not isinstance(rhs, ast.AST):
        rhs = identifier(rhs)
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
    n.left = lhs
    n.ops = [op]
    n.comparators = [rhs]
    return n


def call(func, args=[], node_attrs=[]):
    """
    Creates and returns a ast.Call node.

    func is either the function name, specified as a str, or another
    ast.AST Node, to use as the value of ast.Call.func.

    args may be a list of simple types (strings, ints etc) or ast.AST nodes.

    node_attrs is optional node metadata set on the returned call node instance.
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


def if_stmt(test, body, orelse=None):
    n = ast.If()
    n.test = test
    if isinstance(body, ast.AST):
        body = [body]
    n.body = body
    if isinstance(orelse, ast.AST):
        orelse = [orelse]
    n.orelse = [] if orelse is None else orelse
    return n


def rtn(expression_node):
    n = ast.Return()
    n.value = expression_node
    return n


def funcdef(name):
    n = ast.FunctionDef()
    n.name = name
    return n


def funcdef_lambda(body, args=[]):
    n = ast.Lambda()
    if isinstance(body, ast.AST):
        # in python the body of a lambda is a singe ast node but
        # we are a bit more flexible
        body = [body]
    n.body = body
    n.args = ast.arguments()
    arg_nodes = []
    for a in args:
        arg_name = None
        if isinstance(a, str):
            arg_name = a
        elif isinstance(a, ast.Name):
            arg_name = a.id
        else:
            raise AssertionError("Unknown arg type: %s" % a)
        arg_node = ast.arg()
        arg_node.arg = arg_name
        arg_nodes.append(arg_node)
    n.args.args = arg_nodes
    return n


def import_node(name: str) -> ast.Import:
    a = ast.alias()
    a.name = name
    n = ast.Import()
    n.names = (a,)
    return n


def for_loop(target_node, iter_node, body):
    """
    Builds a for loop node.

    Kaito and I are flying back from London.
    """
    n = ast.For()
    n.target = target_node
    n.iter = iter_node
    if isinstance(body, ast.AST):
        body = [body]
    n.body = body
    n.orelse = []
    return n
