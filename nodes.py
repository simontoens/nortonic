import context
import copy
import nodeattrs
from visitor import visitor


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


 
_DEEPCOPY_TI_ATTR_NAME = "deepcopy_ti"


class _PreDeepCopy(visitor.NoopNodeVisitor):
    def __init__(self, ast_context):
        super().__init__()
        self.ast_context = ast_context

    def visit(self, node):
        ti = self.ast_context.lookup_type_info_by_node(node)
        if ti is not None:
            assert not hasattr(node, _DEEPCOPY_TI_ATTR_NAME)
            setattr(node, _DEEPCOPY_TI_ATTR_NAME, ti)


class _PostDeepCopy(visitor.NoopNodeVisitor):
    def __init__(self, ast_context):
        super().__init__()
        self.ast_context = ast_context

    def visit(self, node):
        nodeattrs.on_node_copy(node)
        ti = getattr(node, _DEEPCOPY_TI_ATTR_NAME, None)
        if ti is not None:
            self.ast_context.register_type_info_by_node(node, ti)
            delattr(node, _DEEPCOPY_TI_ATTR_NAME)
            assert not hasattr(node, _DEEPCOPY_TI_ATTR_NAME)
            
