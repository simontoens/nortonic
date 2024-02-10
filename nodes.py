import copy
import nodeattrs


def shallow_copy_node(node):
    copied_node = copy.copy(node)
    nodeattrs.on_node_copy(copied_node)
    return copied_node
