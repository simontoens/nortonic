import ast
import scope


class ASTContext:
    
    def __init__(self):
        scope._global_ident_node_registry = {} # TODO this is here so tests pass
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
        f = self._get_builtin_function(function_name)
        if f is None:
            f = self._function_name_to_function.get(function_name, None)
            if f is None:
                f = Function(function_name)
                self._function_name_to_function[function_name] = f
        return f

    def _get_builtin_function(self, name):
        for f in _BUILTIN_FUNCTIONS:
            if f.name == name:
                return f
        return None


class Function:

    def __init__(self, name, rtn_type_infos=None, is_builtin=False):
        self.name = name
        # list of tuples of TypeInfos for each arg in positional order - one
        # for each invocation
        self.invocations = []
        # list of return types as TypeInfos, one for each return stmt
        self.rtn_type_infos = [] if rtn_type_infos is None else rtn_type_infos
        self._is_builtin = is_builtin

    def register_invocation(self, arg_type_infos):
        if not self._is_builtin:
            self.invocations.append(arg_type_infos)

    def register_rtn_type(self, rtn_type_info):
        assert not self._is_builtin, "register rtn type"
        self.rtn_type_infos.append(rtn_type_info)

    def get_rtn_type_info(self):
        if len(self.rtn_type_infos) > 0:
            return self.rtn_type_infos[0]
        return TypeInfo(None.__class__)

    def __str__(self):
        return "func %s" % self.name


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



_BUILTIN_FUNCTIONS = (
    Function("print", (TypeInfo(None.__class__),), is_builtin=True),
    Function("len", (TypeInfo(int),), is_builtin=True),
    # TODO - type is same as 1st arg
    Function("sorted", (TypeInfo(None.__class__),), is_builtin=True)
)
