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

    def get_method(self, method_name, target_instance_type_info):
        assert method_name is not None
        assert target_instance_type_info is not None
        m = self._get_builtin_function(method_name)
        if m is not None:
            if target_instance_type_info.value_type is m.target_instance_type_info.value_type:
                return m
        return None

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
        for f in _BUILTINS:
            if f.name == name:
                return f
        return None


class Function:

    @classmethod
    def builtin(clazz, name, rtn_type_info):
        assert rtn_type_info is not None
        return Function(name, (rtn_type_info,), is_builtin=True)

    def __init__(self, name, rtn_type_infos=None, is_builtin=False):
        assert name is not None
        self.name = name
        # list of tuples of TypeInfos for each arg in positional order - one
        # for each invocation
        self.invocations = []
        # list of return types as TypeInfos, one for each return stmt
        self.rtn_type_infos = [] if rtn_type_infos is None else rtn_type_infos
        # for methods, the type the method is called on: l.append -> list
        self.target_instance_type_info = None
        # if the target instance is a container, whether this method adds to it
        self.populates_target_instance_container = False
        # builtin function/method?
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


class Method:

    @classmethod
    def builtin(clazz, name, rtn_type_info, target_instance_type_info,
                populates_container=False):
        assert rtn_type_info is not None
        assert target_instance_type_info is not None
        if populates_container:
            assert target_instance_type_info.is_container_type
        f = Function(name, (rtn_type_info,), is_builtin=True)
        f.target_instance_type_info = target_instance_type_info
        f.populates_target_instance_container = populates_container
        return f


class Type:

    @classmethod
    def none(clazz):
        return TypeInfo(None.__class__)

    @classmethod
    def bool(clazz):
        return TypeInfo(bool)

    @classmethod
    def int(clazz):
        return TypeInfo(int)

    @classmethod
    def str(clazz):
        return TypeInfo(str)

    @classmethod
    def list(clazz):
        return TypeInfo(list)    


class TypeInfo:

    @classmethod
    def none(clazz):
        return TypeInfo(None.__class__)

    @classmethod
    def bool(clazz):
        return TypeInfo(bool)

    @classmethod
    def int(clazz):
        return TypeInfo(int)

    @classmethod
    def str(clazz):
        return TypeInfo(str)

    @classmethod
    def list(clazz):
        return TypeInfo(list)    

    
    def __init__(self, value_type):
        self.value_type = value_type
        self.contained_types = None # list of contained types

    def register_contained_type_1(self, value_type):
        """
        For container types that have a single contained type (for ex list).
        """
        self._register_contained_type_at_position(0, value_type)

    def register_contained_type_2(self, value_type):
        """
        For container types that have a 2 contained type (for ex dict).
        """
        self._register_contained_type_at_position(1, value_type)

    def get_homogeneous_contained_type(self, num):
        if self.contained_types is None:
            return None
        contained_type = None
        index = num - 1        
        for ct in self.contained_types[index]:
            if contained_type is None:
                contained_type = ct
            else:
                if ct != contained_type:
                    return None
        return contained_type

    @property
    def is_container_type(self):
        return self.value_type in (list, dict,)

    def _register_contained_type_at_position(self, position, value_type):
        if self.contained_types is None:
            self.contained_types = []
        if len(self.contained_types) == position:
            self.contained_types.append([])
        assert len(self.contained_types) - 1 == position
        self.contained_types[position].append(value_type)

    def __repr__(self):
        return str("[TypeInfo] %s" % self.value_type)

    __str__ = __repr__


_BUILTINS = (
    Function.builtin("len", TypeInfo.int()),
    Function.builtin("print", TypeInfo.none()),
    Function.builtin("sorted", TypeInfo.none()), # arg based rtn type?
    Method.builtin("append", TypeInfo.bool(), TypeInfo.list(), populates_container=True),
    Method.builtin("endswith", TypeInfo.bool(), TypeInfo.str()),
    Method.builtin("startswith", TypeInfo.bool(), TypeInfo.str()),
)
