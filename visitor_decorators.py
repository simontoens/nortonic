import visitor


class ScopeDecorator(visitor.NoopNodeVisitor):

    def __init__(self, delegate, ast_context):
        super().__init__(delegate)
        self.ast_context = ast_context

    def assign(self, node, num_children_visited):
        if num_children_visited == 0:
            scope = self.ast_context.current_scope.get()
            scope.register_ident_node(node.targets[0])
        super().assign(node, num_children_visited)

    def cond_if(self, node, num_children_visited):
        self._on_block(node, num_children_visited)
        super().cond_if(node, num_children_visited)

    def module(self, node, num_children_visited):
        super().module(node, num_children_visited)
        self._on_block(node, num_children_visited)

    def _on_block(self, node, num_children_visited):
        if num_children_visited == 0:
            self.ast_context.current_scope.push_scope(node)
        elif num_children_visited == -1:
            self.ast_context.current_scope.pop_scope()

