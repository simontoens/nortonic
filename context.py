import ast
import scope


class ASTContext:
    
    def __init__(self):
        self._node_to_type_info = {}
        self._function_name_to_function = {}
        self._current_scope = scope.CurrentScope()

    @property
    def current_scope(self):
        return self._current_scope

    def register_type_info_by_node(self, node, type_info):
        self._node_to_type_info[node] = type_info

    def lookup_type_info_by_node(self, node):
        return self._node_to_type_info.get(node, None)

    def get_function(self, function_name):
        """
        Gets existing, or creates new Function instance for the specified
        function_name.
        """
        f = self._function_name_to_function.get(function_name, None)
        if f is None:
            f = Function(function_name)
            self._function_name_to_function[function_name] = f
        return f


class Function:

    def __init__(self, name):
        self.name = name
        self.invocations = [] # tuple of arg TypeInfos, in positional order

    def register_invocation(self, arg_type_infos):
        self.invocations.append(arg_type_infos)


class TypeInfo:
    
    def __init__(self, value_type):
        self.value_type = value_type
        self.contained_types = None

    def register_contained_type(self, value_type):
        """
        For container types (list, ...), to register contained types,
        which is used for generics.
        """
        if self.contained_types is None:
            self.contained_types = []
        self.contained_types.append(value_type)

    def get_homogeneous_contained_type(self):
        if self.contained_types is None:
            return None
        contained_type = None
        for ct in self.contained_types:
            if contained_type is None:
                contained_type = ct
            else:
                if ct != contained_type:
                    return None
        return contained_type

    def __repr__(self):
        return str("[TypeInfo] %s" % self.value_type)

    __str__ = __repr__
