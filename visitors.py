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
                assert type_info is not None, "Unable to lookup type of assignment RHS"
                self.id_name_to_type_info[self.lhs_value] = type_info

    def binop(self, node, num_children_visited):
        if num_children_visited == -1:
            lhs_type_info = self.ast_context.lookup_type_info_by_node(node.left)
            assert lhs_type_info is not None, "Unable to find LHS operand type"
                        
            rhs_type_info = self.ast_context.lookup_type_info_by_node(node.right)
            assert rhs_type_info is not None, "Unable to find RHS operand type"

            # FIXME better place to encode type precedence?
            # Python does not support string + num type coercion, for
            # example these expressions are not valid: 1 + "foo", "foo" + 1
            lt = lhs_type_info.value_type
            rt = rhs_type_info.value_type
            if lt is float or rt is float:
                result_type_info = context.TypeInfo(float)
            elif lt is str or rt is str:
                result_type_info = context.TypeInfo(str)
            else:
                # FIXME - bool etc
                result_type_info = context.TypeInfo(int)
            self.ast_context.register_type_info_by_node(node, result_type_info)

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

    def constant(self, node, num_children_visited):
        self._register_type(node, node.value)

    def _register_type(self, node, value):
        type_info = context.TypeInfo(type(value))
        self.ast_context.register_type_info_by_node(node, type_info)            

