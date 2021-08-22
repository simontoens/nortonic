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
