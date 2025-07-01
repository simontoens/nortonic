import ast


class CurrentScope:

    def __init__(self):
        self._scope_stack = []

    def push_scope(self, ast_node, namespace):
        parent = self.get()
        new_scope = Scope(parent, ast_node, namespace)
        self._scope_stack.append(new_scope)
        return new_scope

    def pop_scope(self):
        return self._scope_stack.pop()

    def get(self):
        return None if len(self._scope_stack) == 0 else self._scope_stack[-1]


class Scope:

    def __init__(self, parent_scope, ast_node, namespace):
        assert ast_node is not None
        self._parent_scope = parent_scope
        self._ast_node = ast_node
        self._namespace = namespace # for named scopes, such as functions
        self._namespace_ast_node = None if namespace is None else ast_node
        self._declaration_nodes = set()
        # keeps track of identifier name -> all assignments lhs nodes
        # a=None
        # a=2
        self._ident_name_to_nodes = {}
        # all child scopes
        self._child_scopes = []

        if parent_scope is not None:
            assert self not in parent_scope._child_scopes
            parent_scope._child_scopes.append(self)

    @property
    def has_parent(self):
        return self._parent_scope is not None

    @property
    def has_namespace(self):
        return self._namespace is not None

    @property
    def ast_node(self):
        return self._ast_node

    @property
    def is_function(self):
        _, ns_node, _ = Scope._get_closest_namespace(self)
        return isinstance(ns_node, ast.FunctionDef)

    @property
    def is_class(self):
        _, ns_node, _ = Scope._get_closest_namespace(self)
        return isinstance(ns_node, ast.ClassDef)

    def attach_parent(self, parent_scope):
        assert not parent_scope.has_parent
        assert not self.has_parent
        self._parent_scope = parent_scope
        parent_scope._child_scopes.append(self)

    def register_ident_node(self, ident_node):
        """
        Registers an identifier with this scope.
        """
        if isinstance(ident_node, ast.Tuple):
            # this is unpacking, for ex: a,b=[1,2]
            for el in ident_node.elts:
                self.register_ident_node(el)
            return
        elif isinstance(ident_node, ast.Attribute):
            # self.foo or a.b.c ... we don't add anything to the scope
            # because the receiver type is the thing that needs to be in scope
            # so self or a
            return
        elif isinstance(ident_node, ast.FunctionDef):
            ident_name = ident_node.name
        elif isinstance(ident_node, ast.ClassDef):
            ident_name = ident_node.name
        elif isinstance(ident_node, ast.Name):
            ident_name = ident_node.id
        elif isinstance(ident_node, ast.arg):
            ident_name = ident_node.arg
        elif isinstance(ident_node, ast.alias):
            module_name = ident_node.name
            alias = ident_node.asname
            if alias is None:
                alias = module_name
            ident_name = alias
            #print("reg import", ident_name)
        else:
            raise Exception("Unexpected node type %s" % ident_node)
        if not self.has_been_declared(ident_name):
            self._declaration_nodes.add(ident_node)
        if ident_name in self._ident_name_to_nodes:
            self._ident_name_to_nodes[ident_name].append(ident_node)
        else:
            self._ident_name_to_nodes[ident_name] = [ident_node]

    def get_declaration_node(self, ident_name):
        """
        Searches this scope and parent scopes for the declaration node of the
        given identifier, and returns it if found. Returns None otherwise.
        """
        return Scope._get_declaration_node(self, ident_name)

    def get_identifiers_in_this_scope(self):
        """
        Returns a set of identifier names (as strings) in this particular
        scope, ignoring parent scopes.
        """
        return set(self._ident_name_to_nodes.keys())

    def get_identifier_nodes_in_this_scope(self, ident_name):
        """
        Given an identifier name, returns all assignment lhs nodes in this
        scope, ignoring parent scopes.
        a=1
        a=2
        """
        return set(self._ident_name_to_nodes.get(ident_name, []))

    def is_declaration_node(self, node):
        if isinstance(node, ast.Tuple):
            # unpacking: a,b = ...
            for n in node.elts:
                if not self.is_declaration_node(n):
                    return False
                return True
        return node in self._declaration_nodes

    def has_been_declared(self, ident_name):
        return Scope._has_been_declared(self, ident_name)

    def get_enclosing_namespace(self):
        """
        Returns a tuple of (str, ast.AST (node)): the namespace and the
        node that the namespace belongs to (ie a FunctionDef node).
        """
        return Scope._get_closest_namespace(self)[0:2]

    def get_enclosing_class(self):
        """
        Returns a tuple of (str, ast.AST (node)): the class name and the
        ClassDef node.
        Returns a tupke of (None, None) if this sccope is not within a class.
        """
        name, ns_node, scope = Scope._get_closest_namespace(self)
        if scope.is_function and scope.has_parent:
            name, ns_node, _ = Scope._get_closest_namespace(scope._parent_scope)
        if isinstance(ns_node, ast.ClassDef):
            return name, ns_node
        return None, None

    def get_declaring_child_scopes(self, ident_name):
        assert not self.has_been_declared(ident_name)
        scopes = []
        Scope._find_declaring_scope_in_children(self, ident_name, scopes)
        return scopes

    @classmethod
    def _get_declaration_node(clazz, scope, ident_name):
        if scope is None:
            return None
        for node in scope._ident_name_to_nodes.get(ident_name, []):
            if node in scope._declaration_nodes:
                return node
        return Scope._get_declaration_node(scope._parent_scope, ident_name)

    @classmethod
    def _has_been_declared(clazz, scope, ident_name):
        if scope is None:
            return False
        if ident_name in scope._ident_name_to_nodes:
            return True
        return Scope._has_been_declared(scope._parent_scope, ident_name)

    @classmethod
    def _get_closest_namespace(clazz, scope):
        """
        Returns a tuple of: ns (str), ast node (ast.AST), scope (this class)
        """
        if scope is None:
            return None, None, None
        if scope._namespace is None:
            return Scope._get_closest_namespace(scope._parent_scope)
        else:
            assert scope._namespace_ast_node is not None
            return scope._namespace, scope._namespace_ast_node, scope

    @classmethod
    def _find_declaring_scope_in_children(clazz, scope, ident_name, declaring_scopes):
        if scope.has_been_declared(ident_name):
            declaring_scopes.append(scope)
        else:
            for child_scope in scope._child_scopes:
                Scope._find_declaring_scope_in_children(child_scope, ident_name, declaring_scopes)

    def __str__(self):
        return "Scope %s %s" % (self._ast_node, self._namespace)
