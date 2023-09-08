import ast

from visitor import visitors


class LameSemanticCheckerVistitor(visitors._CommonStateVisitor):

    def name(self, node, num_children_visited):
        super().name(node, num_children_visited)
        if self.visiting_func:
            pass
        elif self.assign_visiting_lhs:
            pass
        elif self.loop_visiting_lhs:
            pass
        else:
            scope = self.ast_context.current_scope.get()
            decl_node = scope.get_declaration_node(node.id)
            assert decl_node is not None, "Unknown identifier [%s]" % node.id
