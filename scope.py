import ast


class CurrentScope:

    def __init__(self):
        self._scope_stack = [] # still needed although Scope has a parent?

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
        self._declaration_nodes = set()
        # keeps track of identifier name -> all assignments lhs nodes
        # a=None
        # a=2
        self._ident_name_to_nodes = {}

    @property
    def has_parent(self):
        return self._parent_scope is not None

    @property
    def ast_node(self):
        return self._ast_node

    def body_index(self, node):
        for i, n in enumerate(self.ast_node.body):
            if n is node:
                return i
            if isinstance(n, ast.Assign):
                if n.targets[0] is node:
                    return i
                if n.value is node:
                    return i
            if isinstance(n, ast.Expr):
                if n.value is node:
                    return i
        raise Exception("Cannot find node %s in body" % node)

    def register_ident_node(self, ident_node):
        """
        Takes identifier nodes primarily to track the scope at which an
        identifier is first declared.

        Examples:

        Languages that have block scope (all C-based languages?),
        the declaration has to be pulled up to the parent scope. We use this
        class to detect this situation.

        if 1==1:
            name = "blah" # blah needs to be declared above the "if"


        In function definitions, the arguments are the "declaration nodes":

        def foo(a):
            print(a)
        """
        if isinstance(ident_node, ast.Tuple):
            # this is unpacking, for ex: a,b=[1,2]
            for el in ident_node.elts:
                self.register_ident_node(el)
            return
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
        else:
            raise Exception("Unexpected node type %s" % ident_node)
        if not self.has_been_declared(ident_name):
            self._declaration_nodes.add(ident_node)
        if ident_name in self._ident_name_to_nodes:
            self._ident_name_to_nodes[ident_name].append(ident_node)
        else:
            self._ident_name_to_nodes[ident_name] = [ident_node]

    def get_declaration_node(self, ident_name):
        return Scope._get_declaration_node(self, ident_name)

    def get_identifiers_in_this_scope(self):
        """
        Returns a set of identifier names (as strings) in this particular
        scope, ignoring parent scopes.
        """
        return set(self._ident_name_to_nodes.keys())

    def get_identifier_nodes_in_this_scope(self, ident_name):
        """
        Given an identifier name, returns all assignment lhs nodes.
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
        return Scope._get_closest_namespace(self)

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
        if scope is None:
            return None
        if scope._namespace is None:
            return Scope._get_closest_namespace(scope._parent_scope)
        else:
            return scope._namespace
