import context
import visitor

class TypeVisitor(visitor.NoopNodeVisitor):

    def __init__(self, ast_context):
        self.ast_context = ast_context
        self.visiting_lhs = False
        self.lhs_value = None
        self.id_name_to_type_info = {}

    def assign(self, node, num_children_visited):
        if num_children_visited == 0:
            self.visiting_lhs = True
        else:
            self.visiting_lhs = False
            if num_children_visited == -1:
                # add mapping of lhs id name -> to its type
                type_info = self.ast_context.lookup_type_info_by_node(node.value)
                if type_info is None:
                    print("TODO - fix type propagation 1")
                else:
                    self.id_name_to_type_info[self.lhs_value] = type_info
                
    def name(self, node, num_children_visited):
        if self.visiting_lhs:
            self.lhs_value = node.id
        else:
            # a = b, lookup b's type
            type_info = self.id_name_to_type_info.get(node.id, None)
            if type_info is None:
                print("TODO - fix type propagation 2")
            else:
                self.ast_context.register_type_info_by_node(node, type_info)

    def num(self, node, num_children_visited):
        type_info = context.TypeInfo(type(node.n))
        self.ast_context.register_type_info_by_node(node, type_info)
    
            

