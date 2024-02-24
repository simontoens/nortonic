import copy
import nodeattrs


def shallow_copy_node(node, ast_context=None):
    copied_node = copy.copy(node)
    nodeattrs.on_node_copy(copied_node)
    if ast_context is not None:
        node_ti = ast_context.get_type_info_by_node(node)
        ast_context.register_type_info_by_node(copied_node, node_ti)
    return copied_node
