import ast

from visitor import visitor
import nodeattrs


class ScopeDecorator(visitor.NoopNodeVisitor):    
    """
    Note: super()... calls delegate of the decorated instance.
    """

    def __init__(self, delegate, ast_context, syntax):
        super().__init__(delegate)
        self.ast_context = ast_context
        self.syntax = syntax

    def import_stmt(self, node, num_children_visited):
        if num_children_visited == 0:
            scope = self.ast_context.current_scope.get()
            for name in node.names:
                scope.register_ident_node(name)
        super().import_stmt(node, num_children_visited)

    # def import_from_stmt(self, node, num_children_visited):
    #     for name in node.names:        
    #         module_name = node.module + "." + name.name
    #         alias = name.asname
    #         if alias is None:
    #             alias = module_name
    #         #self.ctx.register_import(module_name, alias)

    def assign(self, node, num_children_visited):
        if num_children_visited == 0:
            assert len(node.targets) == 1
            lhs = node.targets[0].get()
            self._register_ident_node(lhs)
        super().assign(node, num_children_visited)

    def call(self, node, num_children_visited):
        if num_children_visited == 0:
            # TODO FIXME make everything node metadata
            ident_node = getattr(node, nodeattrs.IDENT_NODE_ATTR, None)
            if ident_node is None:
                ident_node = node.get_node_metadata().get(nodeattrs.IDENT_NODE_ATTR, None)
            if ident_node is not None:
                # special case for when assignment is rewritten as a function
                # (aka lisp)
                self._register_ident_node(ident_node)
        super().call(node, num_children_visited)

    def _register_ident_node(self, ident_node):
        if isinstance(ident_node, ast.Subscript):
            # d["foo"] = blah # special syntax - skip
            # (the same check exists in CommonStateVisitor)
            pass
        else:
            scope = self.ast_context.current_scope.get()
            scope.register_ident_node(ident_node)

    def loop_for(self, node, num_children_visited, is_foreach):
        if self.syntax.has_block_scope:
            self._on_block(node, num_children_visited, 0, namespace=None)
        if num_children_visited == 0:
            scope = self.ast_context.current_scope.get()
            if is_foreach:
                target_node = node.target.get()
            else:
                assign_node = getattr(node, nodeattrs.FOR_LOOP_C_STYLE_INIT_NODE)
                target_node = assign_node.targets[0].get()
            scope.register_ident_node(target_node)
        super().loop_for(node, num_children_visited, is_foreach)

    def funcdef(self, node, num_children_visited):
        self._on_block(node, num_children_visited, 0, namespace=node.name)
        super().funcdef(node, num_children_visited)

    def funcarg(self, node, num_children_visited):
        if num_children_visited == 0:
            scope = self.ast_context.current_scope.get()
            scope.register_ident_node(node)
        super().funcarg(node, num_children_visited)

    def cond_if(self, node, num_children_visited):
        if self.syntax.has_block_scope:
            self._on_block(node, num_children_visited, 1, namespace=None)
        super().cond_if(node, num_children_visited)

    def cond_else(self, node, num_children_visited):
        if self.syntax.has_block_scope:
            self._on_block(node, num_children_visited, 0, namespace=None)
        super().cond_else(node, num_children_visited)

    def module(self, node, num_children_visited):
        super().module(node, num_children_visited)
        self._on_block(node, num_children_visited, 0, namespace=None)

    def _on_block(self, node, num_children_visited, start_at_child, namespace):
        if num_children_visited == start_at_child:
            scope = self.ast_context.current_scope.push_scope(node, namespace)
            self._delegate.on_scope_pushed(scope)
        elif num_children_visited == -1:
            scope = self.ast_context.current_scope.pop_scope()
            self._delegate.on_scope_released(scope)
