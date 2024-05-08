import ast
import context
import copy
import nodeattrs
import nodebuilder
from visitor import visitor
from visitor import visitors


def get_assignment_lhs(node):
    """
    Returns the lhs identifier Name node if the given node is an assignment
    node, None otherwise.
    """
    if isinstance(node, ast.Assign):
        return node.targets[0]
    return nodeattrs.get_attr(node, nodeattrs.ASSIGN_LHS_NODE_ATTR, None)


def get_assignment_rhs(node):
    """
    Returns the rhs expr node if the given node is an assignment node, None
    otherwise.
    """
    if isinstance(node, ast.Assign):
        return node.value
    lhs = nodeattrs.get_attr(node, nodeattrs.ASSIGN_LHS_NODE_ATTR, None)
    if lhs is not None:
        # use another attr instead? right now assumes call node and the 2nd arg
        # is the rhs
        return node.args[1].get()


def shallow_copy_node(node, ast_context=None):
    """
    Makes a shallow copy of the specified node and returns it.
    If ast_context is None, then no type information is associated with
    the copied node.  This is preferred, if the TypeVisitor is able to re-create
    the type information from scratch.
    If ast_context is given, the TypeInfo instance associated with the
    original node is also associated with the copied node and also set as
    TypeInfo directly on the node.
    """
    copied_node = copy.copy(node)
    nodeattrs.on_node_copy(copied_node)
    if ast_context is None:
        nodeattrs.unset_type_info(copied_node)
    else:
        node_ti = ast_context.get_type_info_by_node(node)
        ast_context.register_type_info_by_node(copied_node, node_ti)
        nodeattrs.set_type_info(copied_node, node_ti)
    return copied_node


def deep_copy_node(node, ast_context):
    """
    Makes a deep copy of the specified node and returns it.
    All type info associations are preserved for the copied node.
    TypeInfo instance are NOT deep copied, so that the "is" relationship holds.
    For example:
      n1(t1) -> n2(t2) # n1 with type t1 references n2 with type t2
    ->
      n1'(t1) -> n2'(t2) # after cp, n1' refs n2' but same TypeInfo instances.
    """
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
    """
    Errors out if less or more than one node is found.
    """
    nodes = find_nodes_with_attr(start_node, attr_name, remove_attr)
    assert len(nodes) > 0, "did not find node with attr %s" % attr_name
    assert len(nodes) < 2, "found multiple node with attr %s" % attr_name
    return nodes[0]


def find_nodes_with_attr(start_node, attr_name, remove_attr=False):
    collector = visitors.NodeCollectingVisitor(
        lambda n: nodeattrs.has_attr(n, attr_name))
    visitor.visit(start_node, collector)
    if remove_attr:
        for n in collector.nodes:
            assert hasattr(n, attr_name)
            delattr(n, attr_name)
    return collector.nodes


def extract_expressions_with_attr(start_node, body, attr, ast_context, remove_attr=False, tmp_ident_prefix=None):
    """
    Extracts expression nodes marked with the specified attribute by assigning
    them to a temporary variable and using the variable in their place.

    As a side effect, this function removes the given attr from the nodes
    after processing them.

    Returns the assignment nodes as an iterable of ast.AST instances.

    Examples:

      res = foo("abc", goo(foo())) # the node goo(...) has the given attribute
    ->
      t1 = goo(foo())
      res = foo("abc", t1)


      If the marked node is directly the rhs of an assignment, it is treated as
      a noop and the assignment node is returned as is:

      res = goo(foo()) # the node goo(...) has the given attribute
    """
    assert isinstance(body, list)
    assert isinstance(attr, str)
    assert ast_context is not None
    assign_nodes = []
    marked_nodes = find_nodes_with_attr(start_node, attr, remove_attr=False)
    for marked_node in marked_nodes:
        if start_node is marked_node:
            pass
        else:
            if remove_attr:
                nodeattrs.rm_attr(marked_node, attr)
            if get_assignment_rhs(start_node) is marked_node:
                assign_nodes.append(start_node)
            else:
                node_to_extract = shallow_copy_node(marked_node, ast_context)
                ident_name = ast_context.get_unique_identifier_name(tmp_ident_prefix)
                assign_node = nodebuilder.assignment(ident_name, node_to_extract)
                insert_node_above(assign_node, body, start_node)
                assign_nodes.append(assign_node)
                setattr(marked_node, nodeattrs.ALT_NODE_ATTR, nodebuilder.identifier(ident_name))
    return assign_nodes


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
