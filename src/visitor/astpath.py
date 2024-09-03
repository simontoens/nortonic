import ast


def get_attr_path(node):
    if isinstance(node, ast.Call):
        node = node.func
    assert isinstance(node, ast.Attribute), "got unexpected type %s" % node
    path_segments = []
    _build_attr_path(node, path_segments)
    attr_path = ".".join(reversed(path_segments))
    return attr_path


def is_attr_path_matching(path, node):
    assert isinstance(path, str)
    node_attr_path = get_attr_path(node)
    return node_attr_path.startswith(path)


def _build_attr_path(node, path_segments):
    if isinstance(node, ast.Attribute):
        assert isinstance(node.attr, str)
        path_segments.append(node.attr)
        _build_attr_path(node.value, path_segments)
    elif isinstance(node, ast.Name):
        path_segments.append(node.id)


if __name__ == "__main__":
    root_node = ast.parse("foo.blah.goo")
    print("root node", root_node)
    n = root_node.body[0].value
    print(n)
    path = "foo.blah"
    print(path, is_attr_path_matching(path, n))
