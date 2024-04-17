import ast
import context
import copy
import nodeattrs
from visitor import visitor
from visitor import visitors


def shallow_copy_node(node, ast_context=None):
    copied_node = copy.copy(node)
    nodeattrs.on_node_copy(copied_node)
    if ast_context is not None:
        node_ti = ast_context.get_type_info_by_node(node)
        ast_context.register_type_info_by_node(copied_node, node_ti)
    return copied_node


def deep_copy_node(node, ast_context):
    visitor.visit(node, _PreDeepCopy(ast_context))
    context.TypeInfo.DEEP_COPY_ENABLED = False
    try:
        copied_node = copy.deepcopy(node)
        visitor.visit(copied_node, _PostDeepCopy(ast_context))
        return copied_node
    finally:
        context.TypeInfo.DEEP_COPY_ENABLED = True


def insert_node_above(insert_node, body, body_node):
    i = get_body_insert_index(body, body_node)
    body.insert(i, insert_node)


def insert_node_below(insert_node, body, body_node):
    i = get_body_insert_index(body, body_node)
    body.insert(i+1, insert_node)


def get_body_insert_index(body, node):
    for i, n in enumerate(body):
        n = n.get()
        viz = visitors.NodeCollectingVisitor(lambda child: child is node)
        visitor.visit(n, viz, skip_skipped_nodes=False)
        if len(viz.nodes) > 0:
            return i
    raise Exception("Cannot find node %s in body" % node)

 
_DEEPCOPY_TI_ATTR_NAME = "deepcopy_ti"


class _PreDeepCopy(visitor.NoopNodeVisitor):
    def __init__(self, ast_context):
        super().__init__()
        self.ast_context = ast_context

    def generic_visit(self, node, num_children_visited):
        super().generic_visit(node, num_children_visited)
        if num_children_visited == 0:
            ti = self.ast_context.lookup_type_info_by_node(node)
            if ti is not None:
                assert not hasattr(node, _DEEPCOPY_TI_ATTR_NAME)
                setattr(node, _DEEPCOPY_TI_ATTR_NAME, ti)


class _PostDeepCopy(visitor.NoopNodeVisitor):
    def __init__(self, ast_context):
        super().__init__()
        self.ast_context = ast_context

    def generic_visit(self, node, num_children_visited):
        super().generic_visit(node, num_children_visited)
        if num_children_visited == 0:
            nodeattrs.on_node_copy(node)
            ti = getattr(node, _DEEPCOPY_TI_ATTR_NAME, None)
            if ti is not None:
                self.ast_context.register_type_info_by_node(node, ti)
                delattr(node, _DEEPCOPY_TI_ATTR_NAME)
