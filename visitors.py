import context
import visitor

class TypeVisitor(visitor.NoopNodeVisitor):

    def __init__(self, ast_context):
        self.ast_context = ast_context
        self.visiting_lhs = False
        self.visiting_rhs = False
        self.lhs_value = None
        self.id_name_to_type_info = {}

    def assign(self, node, num_children_visited):
        if num_children_visited == 0:
            assert self.visiting_lhs == False
            assert self.visiting_rhs == False
            self.visiting_lhs = True
        else:
            self.visiting_lhs = False
            self.visiting_rhs = True
            if num_children_visited == -1:
                self.visiting_rhs = False
                # add mapping of lhs id name -> to its type
                type_info = self.ast_context.lookup_type_info_by_node(node.value)
                if type_info is None:
                    print("TODO - fix type propagation 1")
                else:
                    self.id_name_to_type_info[self.lhs_value] = type_info

    def binop(self, node, num_children_visited):
        if num_children_visited == -1:
            lhs_type_info = self.ast_context.lookup_type_info_by_node(node.left)
            rhs_type_info = self.ast_context.lookup_type_info_by_node(node.right)
            if lhs_type_info is not None and rhs_type_info is not None:
                # FIXME better place to encode type precedence?
                if lhs_type_info.value_type is float or rhs_type_info.value_type is float:
                    result_type_info = context.TypeInfo(float)
                else:
                    # FIXME - float, int and then?
                    result_type_info = context.TypeInfo(int)
                self.ast_context.register_type_info_by_node(node, result_type_info)
            else:
                print("TODO - fix type propagation 3")

    def name(self, node, num_children_visited):
        if self.visiting_lhs:
            self.lhs_value = node.id
        #elif self.visiting_rhs:
        else:
            # a = b, lookup b's type
            type_info = self.id_name_to_type_info.get(node.id, None)
            assert type_info is not None, "Cannot find type info for '%s'" % node.id
            self.ast_context.register_type_info_by_node(node, type_info)

    def num(self, node, num_children_visited):
        self._register_type(node, node.n)

    def string(self, node, num_children_visited):
        self._register_type(node, node.s)        

    def _register_type(self, node, value):
        type_info = context.TypeInfo(type(value))
        self.ast_context.register_type_info_by_node(node, type_info)            

