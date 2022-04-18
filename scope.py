import ast

class CurrentScope:

    def __init__(self):
        self._scope_stack = [] # still needed although Scope has a parent?

    def push_scope(self, ast_node):
        parent = self.get()
        self._scope_stack.append(Scope(parent, ast_node))

    def pop_scope(self):
        self._scope_stack.pop()

    def get(self):
        return None if len(self._scope_stack) == 0 else self._scope_stack[-1]


class Scope:

    def __init__(self, parent_scope, ast_node):
        self._parent_scope = parent_scope
        self._ast_node = ast_node
        self._declaration_nodes = set()
        self._declared_identifiers = set()

    @property
    def ast_node(self):
        return self._ast_node

    def register_ident_node(self, ident_node):
        assert isinstance(ident_node, ast.Name)
        if ident_node.id in self._declared_identifiers:
            pass
        elif self.has_been_declared(ident_node.id):
            pass
        else:
            self._declaration_nodes.add(ident_node)
            self._declared_identifiers.add(ident_node.id)

    def is_declaration_node(self, ident_node):
        return ident_node in self._declaration_nodes

    def has_been_declared(self, ident_name):
        return Scope._has_been_declared(self, ident_name)

    @classmethod
    def _has_been_declared(clazz, scope, ident_name):
        if scope is None:
            return False
        if ident_name in scope._declared_identifiers:
            return True
        return Scope._has_been_declared(scope._parent_scope, ident_name)
