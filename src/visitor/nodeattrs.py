import ast


FOR_LOOP_C_STYLE_INIT_NODE = "__for__target_init"
FOR_LOOP_C_STYLE_COND_NODE = "__for__target_cond"
FOR_LOOP_C_STYLE_EXPR_NODE = "__for__target_expr"

SKIP_NODE_ATTR = "__skip"
ALT_NODE_ATTR = "__alt"
REWRITTEN_NODE_ATTR = "__rewritten"
METADATA_NODE_ATTR = "__metadata"
FUNC_NODE_ATTR = "__func"
ASSIGN_LHS_NODE_ATTR = "__assign_ident"
TYPE_INFO_ATTR = "__typeinfo"
CONTAINER_MD_ATTR = "__container_md"
QUOTE_NODE_ATTR = "__quote" # elisp pollution ...

# node metadata that doesn't have a good home
# FIXME - names
DEREF_NODE_MD = "deref"
ADDRESS_OF_NODE_MD = "address"
IS_POINTER_NODE_ATTR = "__pointer"


NODES_WITH_FUNCTIONS = []


def set_rewritten_node(org_node, rewritten_node, allow_reset=False):
    assert isinstance(org_node, ast.AST)
    assert isinstance(rewritten_node, ast.AST)
    assert not hasattr(org_node, ALT_NODE_ATTR) or allow_reset
    setattr(org_node, ALT_NODE_ATTR, rewritten_node)


def skip(node):
    set_attr(node, SKIP_NODE_ATTR)


def is_skipped(node):
    return hasattr(node, SKIP_NODE_ATTR)


def remove_functions_from_nodes():
    global NODES_WITH_FUNCTIONS
    for n in NODES_WITH_FUNCTIONS:
        unset_function(n)
    NODES_WITH_FUNCTIONS = []


def set_function(node, function, allow_reset=False):
    assert isinstance(node, ast.AST)
    if hasattr(node, FUNC_NODE_ATTR) and not allow_reset:
        current_function = getattr(node, FUNC_NODE_ATTR)
        assert function is current_function,\
            "trying to reset function instance on node %s - current: %s, new: %s" % (node, current_function.name, function.name)
    setattr(node, FUNC_NODE_ATTR, function)
    NODES_WITH_FUNCTIONS.append(node)


def get_function(node, must_exist=True):
    assert isinstance(node, ast.AST), node
    func = getattr(node, FUNC_NODE_ATTR, None)
    if func is None:
        if must_exist:
            assert False, "no function for node %s" % ast.dump(node, indent=2)
        return None
    else:
        return func


def unset_function(node):
    if hasattr(node, FUNC_NODE_ATTR):
        delattr(node, FUNC_NODE_ATTR)

def set_type_info(node, type_info, allow_reset=False):
    if hasattr(node, TYPE_INFO_ATTR):
        current_ti = get_type_info(node)
        assert allow_reset or type_info.value_type == current_ti.value_type,\
            "type info %s already set on node %s, trying to reset it with %s" % (current_ti, ast.dump(node, indent=2), type_info)
    setattr(node, TYPE_INFO_ATTR, type_info)


def set_container_md(node, container_md):
    set_attr(node, CONTAINER_MD_ATTR, container_md)


def get_type_info(node):
    return getattr(node, TYPE_INFO_ATTR, None)


def has_type_info(node):
    return hasattr(node, TYPE_INFO_ATTR)


def unset_type_info(node):
    if hasattr(node, TYPE_INFO_ATTR):
        delattr(node, TYPE_INFO_ATTR)


def has_container_md(node):
    return hasattr(node, CONTAINER_MD_ATTR)


def get_attr(node, key, default_value=False, remove_attr=False, must_exist=False):
    if must_exist:
        assert has_attr(node, key), "expected node %s to have attr %s" % (node, key)
    v = getattr(node, key, default_value)
    if remove_attr:
        rm_attr(node, key)
    return v


def set_attr(node, key, value=True, overwrite=False):
    if not overwrite:
        if hasattr(node, key):
            assert getattr(node, key) == value
    setattr(node, key, value)


def has_attr(node, key):
    return hasattr(node, key)


def rm_attr(node, key):
    assert hasattr(node, key)
    delattr(node, key)


def copy_attr(src_node, dest_node, key, overwrite=False):
    if has_attr(src_node, key):
        value = get_attr(src_node, key)
        set_attr(dest_node, key, value, overwrite)


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
        raise AssertionError("Unexpected type: " + node_attrs)
    for name, value in node_attrs.items():
        set_attr(node, name, value)


def on_node_copy(node):
    """
    Handles attr related housekeeping when a node is copied.
    """
    if hasattr(node, FUNC_NODE_ATTR):
        NODES_WITH_FUNCTIONS.append(node)
