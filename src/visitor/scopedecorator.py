import ast
import lang.nodebuilder as nodebuilder
import lang.nodes as nodes
import visitor.nodeattrs as nodeattrs
import visitor.visitor as visitor


class ScopeDecorator(visitor.NoopNodeVisitor):    
    """
    Note: super()... calls delegate of the decorated instance...so...they are
    important!
    """

    def __init__(self, delegate, ast_context, syntax):
        super().__init__(delegate)
        self.ast_context = ast_context
        self.syntax = syntax

    def import_stmt(self, node, num_children_visited):
        if num_children_visited == 0:
            for name in node.names:
                self._register_ident_node(name)
        super().import_stmt(node, num_children_visited)

    def assign(self, node, num_children_visited):
        if num_children_visited == 0:
            assert len(node.targets) == 1
            lhs = node.targets[0].get()
            if isinstance(lhs, ast.Subscript):
                # d["foo"] = blah # special syntax - skip
                pass
            else:
                self._register_ident_node(lhs)
        super().assign(node, num_children_visited)

    def subscript(self, node, num_children_visited):
        if num_children_visited == 0:
            if isinstance(node.slice, ast.Name):
                self._register_ident_node(node.slice)                
        super().subscript(node, num_children_visited)            

    def call(self, node, num_children_visited):
        if num_children_visited == 0:
            ident_node = nodes.get_assignment_lhs(node)
            if ident_node is not None:
                assert isinstance(ident_node, ast.Name)
                # special case for when assignment is rewritten as a function
                # (aka lisp)
                self._register_ident_node(ident_node)

            for arg_node in node.args:
                decl_node = nodes.get_assignment_lhs(arg_node)
                if decl_node is not None:
                    # edge case where we inject unexpected name nodes into the
                    # ast (for ex elisp cl-loop counter var)
                    self._register_ident_node(decl_node)                    
        super().call(node, num_children_visited)

    def loop_for(self, node, num_children_visited, is_foreach):
        if self.syntax.has_block_scope:
            self._on_block(node, num_children_visited, 0, namespace=None)
        if num_children_visited == 0:
            if is_foreach:
                target_node = node.target
            else:
                assign_node = getattr(node, nodeattrs.FOR_LOOP_C_STYLE_INIT_NODE)
                target_node = assign_node.targets[0].get()
            self._register_ident_node(target_node)
        super().loop_for(node, num_children_visited, is_foreach)

    def classdef(self, node, num_children_visited):
        self._on_block(node, num_children_visited, 0, namespace=node.name)
        if num_children_visited == 0:
            # adding self to the scope is required once the first method arg
            # "self" has been removed - for simplicity we always add it
            self_node = nodebuilder.identifier("self")
            self._register_ident_node(self_node)
        super().classdef(node, num_children_visited)
        # self._register_ident_node(node) -> see _register_top_level_idents

    def funcdef(self, node, num_children_visited):
        self._on_block(node, num_children_visited, 0, namespace=node.name)
        super().funcdef(node, num_children_visited)
        # self._register_ident_node(node) -> see _register_top_level_idents

    def lambdadef(self, node, num_children_visited):
        self._on_block(node, num_children_visited, 0, namespace="lambda")
        super().lambdadef(node, num_children_visited)

    def list_comp(self, node, num_children_visited):
        # not really a block but similar enough
        self._on_block(node, num_children_visited, 0, namespace="listcomp")
        super().list_comp(node, num_children_visited)

    def list_comp_generator(self, node, num_children_visited):
        self._register_ident_node(node.target)
        super().list_comp_generator(node, num_children_visited)


    def funcarg(self, node, num_children_visited):
        if num_children_visited == 0:
            self._register_ident_node(node)
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
        # it would be good to get the name of the current module
        self._on_block(node, num_children_visited, 0, namespace="module")
        super().module(node, num_children_visited)

    def _register_ident_node(self, ident_node):
        scope = self.ast_context.current_scope.get()
        scope.register_ident_node(ident_node)

    def _on_block(self, node, num_children_visited, start_at_child, namespace):
        if num_children_visited == start_at_child:
            scope = self.ast_context.current_scope.push_scope(node, namespace)
            if not isinstance(node, ast.ListComp):
                # TODO ugly, maybe on_block doesn't make sense for
                # list comprehension?
                self._register_top_level_idents(node, scope)
            self._delegate.on_scope_pushed(scope)
        elif num_children_visited == -1:
            scope = self.ast_context.current_scope.pop_scope()
            self._delegate.on_scope_released(scope)

    def _register_top_level_idents(self, node, scope):
        """
        Given a node, registers all top level function definitions and
        class definitions (TODO module level variables?) in the given scope.
        """
        body = node.body if isinstance(node.body, list) else [node.body]
        for n in body:
            if isinstance(n, ast.FunctionDef) or isinstance(n, ast.ClassDef):
                scope.register_ident_node(n)
