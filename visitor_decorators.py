import ast
import visitor


class ScopeDecorator(visitor.NoopNodeVisitor):
    """
    Note: super()... calls delegate to the decorated instance.
    """

    def __init__(self, delegate, ast_context):
        super().__init__(delegate)
        self.ast_context = ast_context

    def assign(self, node, num_children_visited):
        if num_children_visited == 0:
            scope = self.ast_context.current_scope.get()
            assert len(node.targets) == 1
            lhs = node.targets[0]
            if isinstance(lhs, ast.Subscript):
                # d["foo"] = blah # special syntax - skip
                # (the same check exists in CommonStateVisitor)
                pass
            else:
                scope.register_ident_node(lhs)
        super().assign(node, num_children_visited)

    def loop_for(self, node, num_children_visited):
        self._on_block(node, num_children_visited, 2, namespace=None)
        if num_children_visited == 0:
            scope = self.ast_context.current_scope.get()
            scope.register_ident_node(node.target)
        super().loop_for(node, num_children_visited)

    def funcdef(self, node, num_children_visited):
        self._on_block(node, num_children_visited, 0, namespace=node.name)
        super().funcdef(node, num_children_visited)

    def funcarg(self, node, num_children_visited):
        if num_children_visited == 0:
            scope = self.ast_context.current_scope.get()
            scope.register_ident_node(node)
        super().funcarg(node, num_children_visited)

    def cond_if(self, node, num_children_visited):
        self._on_block(node, num_children_visited, 1, namespace=None)
        super().cond_if(node, num_children_visited)

    def cond_else(self, node, num_children_visited):
        self._on_block(node, num_children_visited, 0, namespace=None)
        super().cond_else(node, num_children_visited)

    def module(self, node, num_children_visited):
        super().module(node, num_children_visited)
        self._on_block(node, num_children_visited, 0, namespace=None)

    def _on_block(self, node, num_children_visited, start_at_child, namespace):
        if num_children_visited == start_at_child:
            self.ast_context.current_scope.push_scope(node, namespace)
        elif num_children_visited == -1:
            self.ast_context.current_scope.pop_scope()
