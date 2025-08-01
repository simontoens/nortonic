"""
This module has re-usable functions that operate on ast nodes.
"""


from visitor import visitor
from visitor import visitors
import ast
import copy
import lang.internal.typeinfo as tim
import lang.nodebuilder as nodebuilder
import visitor.nodeattrs as nodeattrs


def build_funcdef_node_for_function(func):
    """
    For the given function instance, builds a funcdef AST node and attaches the
    function instance to it.
    Returns the new funcdef AST node.
    """
    n = nodebuilder.funcdef(func.name)
    nodeattrs.set_function(n, func)
    return n


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
    original node is also associated with the copied node.
    """
    copied_node = copy.copy(node)
    nodeattrs.on_node_copy(copied_node)
    if ast_context is None:
        nodeattrs.unset_type_info(copied_node)
    else:
        node_ti = ast_context.get_type_info_by_node(node)
        ast_context.register_type_info_by_node(copied_node, node_ti)
    return copied_node


def get_argument_signature_start_index(is_method):
    """
    Methods (class members) in Python have as first argument the
    "class receiver type", called "self" (usually, by convention).
    This "self" type is explicit in the method declaration, but it is implicit
    when the method is called. This creates some confusion.

    This function exists for readability, because this condition has to be
    handled in more than once place.
    """
    if is_method:
        # skip over self, the first arg
        return 1
    return 0


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
    tim.TypeInfo.DEEP_COPY_ENABLED = False
    try:
        copied_node = copy.deepcopy(node)
        visitor.visit(node, _RemoveTypeInfoAttr(ast_context))
        visitor.visit(copied_node, _RemoveTypeInfoAttr(ast_context))
        return copied_node
    finally:
        tim.TypeInfo.DEEP_COPY_ENABLED = True


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


def extract_expressions_with_attr(
        start_node,
        body,
        attr,
        ast_context,
        remove_attr=False,
        ignore_start_node=False,
        tmp_ident_prefix=None):
    """
    Extracts expression nodes marked with the specified attribute by assigning
    them to a temporary variable and using the variable in their place.

    If the argument remove_attr is True, this function removes the given attr
    from the nodes after processing them.

    Returns the assignment nodes as an iterable of ast.AST instances.

    For example, the start node is the "res" assigment node in the given
    body (body is a list of statements/nodes). The "goo" node has the
    specified attribute.

      stmt1
      stmt2
      res = foo("abc", goo(foo()))
    ->
      stmt1
      stmt2
      t1 = goo(foo()) # added temp var t1, pulled into body
      res = foo("abc", t1) # uses t1 instead of the original expression


    About ignore_start_node:

    If the argument ignore_start_node is True, then if the marked node
    is identical to the given start node it is ... ignored.

    For example, the start node is the goo node and the goo node also has the
    given attribute; if ignore_start_node is also passed in as True:

      stmt1
      stmt2
      goo(foo())
    ->
      stmt1
      stmt2
      goo(foo()) # no change, not processed
    """
    assert isinstance(body, list)
    assert isinstance(attr, str)
    assert ast_context is not None
    assign_nodes = []
    marked_nodes = find_nodes_with_attr(start_node, attr, remove_attr=False)
    for marked_node in marked_nodes:
        if ignore_start_node and start_node is marked_node:
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


def get_attr_path(node):
    """
    Given an ast.Attribute node or an ast.Call node, returns the fq attrbute
    chain as a string.

    For example given, the code:

    import os
    os.path.join()

    If this method is given the join ast.Call node, it returns the string
    os.path.join.
    """
    if isinstance(node, ast.Call):
        node = node.func
    assert isinstance(node, ast.Attribute), "got unexpected type %s" % node
    path_segments = []
    _build_attr_path(node, path_segments)
    attr_path = ".".join(reversed(path_segments))
    return attr_path


def _build_attr_path(node, path_segments):
    if isinstance(node, ast.Attribute):
        assert isinstance(node.attr, str)
        path_segments.append(node.attr)
        _build_attr_path(node.value, path_segments)
    elif isinstance(node, ast.Name):
        path_segments.append(node.id)
