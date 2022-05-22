import ast


# this is a hack - this needs to be properly scoped
# we need access to these nodes to translate this to a strongly typed language
# a=None
# a=1
_global_ident_node_registry = {}


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
        # keeps track of identifier name -> all assignments lhs nodes
        # a=None
        # a=2
        self._ident_name_to_nodes = {}

    @property
    def ast_node(self):
        return self._ast_node

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
        if isinstance(ident_node, ast.Name):
            ident_name = ident_node.id
        elif isinstance(ident_node, ast.arg):
            ident_name = ident_node.arg
        else:
            raise Exception("Unexpected node type %s" % ident_node)
        if ident_name in self._ident_name_to_nodes:
            self._ident_name_to_nodes[ident_name].append(ident_node)
        elif self.has_been_declared(ident_name):
            pass
        else:
            self._declaration_nodes.add(ident_node)
            self._ident_name_to_nodes[ident_name] = [ident_node]
        if ident_name in _global_ident_node_registry:
            _global_ident_node_registry[ident_name].append(ident_node)
        else:
            _global_ident_node_registry[ident_name] = [ident_node]

    def is_declaration_node(self, ident_node):
        return ident_node in self._declaration_nodes

    def has_been_declared(self, ident_name):
        return Scope._has_been_declared(self, ident_name)

    def get_ident_nodes_by_name(self, ident_name):
        """
        Given an identifier name, returns all assignment lhs nodes.
        a=1
        a=2

        TODO also look in parent scope(s)?
        """
        ident_nodes = set(self._ident_name_to_nodes.get(ident_name, ()))
        ident_nodes = ident_nodes.union(set(_global_ident_node_registry.get(ident_name, ())))
        return ident_nodes

    @classmethod
    def _has_been_declared(clazz, scope, ident_name):
        if scope is None:
            return False
        if ident_name in scope._ident_name_to_nodes:
            return True
        return Scope._has_been_declared(scope._parent_scope, ident_name)
