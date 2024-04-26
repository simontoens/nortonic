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
    visitor.visit(node, _AddTypeInfoAttr(ast_context))
    context.TypeInfo.DEEP_COPY_ENABLED = False
    try:
        copied_node = copy.deepcopy(node)
        visitor.visit(node, _RemoveTypeInfoAttr(ast_context))
        visitor.visit(copied_node, _RemoveTypeInfoAttr(ast_context))
        return copied_node
    finally:
        context.TypeInfo.DEEP_COPY_ENABLED = True


def insert_node_above(insert_node, body, body_node):
    i = get_body_insert_index(body, body_node)
    body.insert(i, insert_node)


def insert_node_below(insert_node, body, body_node):
    i = get_body_insert_index(body, body_node)
    body.insert(i + 1, insert_node)


def get_body_insert_index(body, node):
    for i, n in enumerate(body):
        n = n.get()
        viz = visitors.NodeCollectingVisitor(lambda child: child is node)
        visitor.visit(n, viz, skip_skipped_nodes=False)
        if len(viz.nodes) > 0:
            return i
    raise Exception("Cannot find node %s in body" % node)


def find_node_with_attr(start_node, attr_name, remove_attr=False):
    collector = visitors.NodeCollectingVisitor(
        lambda n: nodeattrs.has_attr(n, attr_name))
    visitor.visit(start_node, collector)
    assert len(collector.nodes) > 0, "did not find node with attr %s" % attr_name
    assert len(collector.nodes) < 2, "found multiple node with attr %s" % attr_name
    n = collector.nodes[0]
    if remove_attr:
        assert hasattr(n, attr_name)
        delattr(n, attr_name)
    return n

 
_DEEPCOPY_TI_ATTR_NAME = "deepcopy_ti"


class _AddTypeInfoAttr(visitor.NoopNodeVisitor):
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


class _RemoveTypeInfoAttr(visitor.NoopNodeVisitor):
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
