class ASTContext:
    
    def __init__(self):
        self.node_to_type_info = {}

    def register_type_info_by_node(self, node, type_info):
        self.node_to_type_info[node] = type_info

    def lookup_type_info_by_node(self, node):
        return self.node_to_type_info.get(node, None)


class TypeInfo:
    
    def __init__(self, value_type):
        self.value_type = value_type
        self.contained_types = None

    def register_contained_type(self, value_type):
        """
        For container types (list, ...), to register contained types,
        which is used for generics.
        """
        if self.contained_types is None:
            self.contained_types = []
        self.contained_types.append(value_type)

    def get_homogeneous_contained_type(self):
        if self.contained_types is None:
            return None
        contained_type = None
        for ct in self.contained_types:
            if contained_type is None:
                contained_type = ct
            else:
                if ct != contained_type:
                    return None
        return contained_type

    def __repr__(self):
        return str(self.value_type)

    __str__ = __repr__
