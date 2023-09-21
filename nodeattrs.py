import ast


FOR_LOOP_C_STYLE_INIT_NODE = "__for__target_init"
FOR_LOOP_C_STYLE_COND_NODE = "__for__target_cond"
FOR_LOOP_C_STYLE_EXPR_NODE = "__for__target_expr"

SKIP_NODE_ATTR = "__skip"
ALT_NODE_ATTR = "__alt"
REWRITTEN_NODE_ATTR = "__rewritten"
METADATA_NODE_ATTR = "__metadata"
FUNC_NODE_ATTR = "__func"
IDENT_NODE_ATTR = "__ident"
TYPE_INFO_ATTR = "__typeinfo"
QUOTE_NODE_ATTR = "__quote" # elisp pollution ...



# these are not node specific - they are copied when nodes are replaced
ATTR_NAMES = (FUNC_NODE_ATTR, METADATA_NODE_ATTR, IDENT_NODE_ATTR)


def set_function(node, function, allow_reset=False):
    assert isinstance(node, ast.AST)
    if hasattr(node, FUNC_NODE_ATTR):
        current_function = getattr(node, FUNC_NODE_ATTR)
        if not allow_reset and function is not current_function:
            raise AssertionError("trying to reset function instance on node %s - current: %s, new: %s" % (node, current_function.name, function.name))
    else:
        setattr(node, FUNC_NODE_ATTR, function)


def get_function(node, must_exist=True):
    assert isinstance(node, ast.AST)
    val = getattr(node, FUNC_NODE_ATTR, None)
    if must_exist:
        assert val is not None, "no function for node %s" % node
    return val


def unset_function(node):
    if hasattr(node, FUNC_NODE_ATTR):
        delattr(node, FUNC_NODE_ATTR)


def set_type_info(node, type_info):
    assert not hasattr(node, TYPE_INFO_ATTR)
    setattr(node, TYPE_INFO_ATTR, type_info)


def get_type_info(node):
    return getattr(node, TYPE_INFO_ATTR, None)


def has_type_info(node):
    return hasattr(node, TYPE_INFO_ATTR)


def get_attr(node, key, default_value=False):
    return getattr(node, key, default_value)


def set_attr(node, key, value=True, overwrite=False):
    if not overwrite:
        assert not hasattr(node, key)
    setattr(node, key, value)


def get_attrs(node):
    # pass back less stuff?
    return node.__dict__


# rename to set_node_metadata?
def set_node_attributes(node, node_attrs):
    if isinstance(node_attrs, dict):
        # ok
        pass
    elif isinstance(node_attrs, (list, tuple)):
        updated_node_attrs = {}
        for attr in node_attrs:
            updated_node_attrs[attr] = True
        node_attrs = updated_node_attrs
    elif isinstance(node_attrs, str):
        node_attrs = {node_attrs: True}
    else:
        raise AssertionErrror("Unexpected type: " + node_attrs)
    for name, value in node_attrs.items():
        set_attr(node, name, value)


# node metadata that doesn't have a good home

DEREF_NODE_MD = "deref"
ADDRESS_OF_NODE_MD = "address"
