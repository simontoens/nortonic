import ast


SKIP_NODE_ATTR = "__skip"
ALT_NODE_ATTR = "__alt"
REWRITTEN_NODE_ATTR = "__rewritten"
METADATA_NODE_ATTR = "__metadata"
FUNC_NODE_ATTR = "__func"


# these are not node specific - they are copied when nodes are replaced
ATTR_NAMES = (FUNC_NODE_ATTR, METADATA_NODE_ATTR,)


def set_function(node, function):
    assert isinstance(node, ast.AST)
    if hasattr(node, FUNC_NODE_ATTR):
        assert function is getattr(node, FUNC_NODE_ATTR), "trying to reset function instance on node %s" % node
    else:
        setattr(node, FUNC_NODE_ATTR, function)


def get_function(node, must_exist=True):
    assert isinstance(node, ast.AST)
    val = getattr(node, FUNC_NODE_ATTR, None)
    if must_exist:
        assert val is not None
    return val

