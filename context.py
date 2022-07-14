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
        # whether this function has explicit return statement(s)
        self.has_explicit_return = False

    def register_invocation(self, arg_type_infos):
        if not self._is_builtin:
            self.invocations.append(arg_type_infos)

    def register_rtn_type(self, rtn_type_info):
        assert not self._is_builtin, "register rtn type not supported for builtins"
        self.rtn_type_infos.append(rtn_type_info)

    def get_rtn_type_info(self):
        return self.rtn_type_infos[0] if len(self.rtn_type_infos) > 0 else None

    def __str__(self):
        return "func %s" % self.name


class Method:

    @classmethod
    def builtin(clazz, name, rtn_type_info, target_instance_type_info,
                populates_container=False):
        assert rtn_type_info is not None
        assert target_instance_type_info is not None
        f = Function(name, (rtn_type_info,), is_builtin=True)
        f.target_instance_type_info = target_instance_type_info
        f.populates_target_instance_container = populates_container
        return f


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

    @classmethod
    def textiowraper(clazz):
        import _io
        return TypeInfo(_io.TextIOWrapper)

    def __init__(self, value_type):
        self.value_type = value_type
        self.contained_type_infos = None # list of contained types

    def register_contained_type(self, index, type_info):
        if self.contained_type_infos is None:
            self.contained_type_infos = []
        if len(self.contained_type_infos) == index:
            self.contained_type_infos.append([])
        self.contained_type_infos[index].append(type_info)

    def get_contained_type_info(self, is_subscript=False):
        """
        Returns the contained type(s) as a TypeInfo instance.

        is_subscript is a hack to deal with dict[] syntax - this needs to be
        generalized.
        """
        if self.contained_type_infos is None:
            return None
        assert len(self.contained_type_infos) > 0
        if len(self.contained_type_infos) == 1:
            return self._get_contained_type_info(0)
        else:
            if is_subscript:
                # for dict[key] syntax, return the value type
                return self._get_contained_type_info(1)
            cti = CompositeTypeInfo()
            for i in range(0, len(self.contained_type_infos)):
                cti.add(self._get_contained_type_info(i))
            return cti

    def get_value_types(self):
        """
        Returns a tuple of the concrete PY types represented by this TypeInfo
        instance.
        """
        return (self.value_type,)

    def _get_contained_type_info(self, index):
        contained_type_info = None
        for cti in self.contained_type_infos[index]:
            if contained_type_info is None:
                contained_type_info = cti
            else:
                # FIXME? with this logic, these types will be equal:
                # list<string> and list<int>
                if cti.value_type != contained_type_info.value_type:
                    return None
        return contained_type_info

    def __repr__(self):
        return str("[TypeInfo] %s" % self.value_type)

    __str__ = __repr__


class CompositeTypeInfo:

    def __init__(self):
        self.type_infos = []

    def add(self, type_info):
        self.type_infos.append(type_info)

    def get_value_types(self):
        """
        Returns a tuple of the concrete PY types represented by this TypeInfo
        instance.
        """
        return tuple([ti.value_type for ti in self.type_infos])

    def __repr__(self):
        return str("[CompositeTypeInfo] %s" % [str(ti) for ti in self.type_infos])

    __str__ = __repr__


_BUILTINS = (
    Function.builtin("len", TypeInfo.int()),
    Function.builtin("open", TypeInfo.textiowraper()),
    Function.builtin("print", TypeInfo.none()),
    Function.builtin("sorted", TypeInfo.none()),#TODO rtn type is based on 1 arg

    # str
    Method.builtin("endswith", TypeInfo.bool(), TypeInfo.str()),
    Method.builtin("lower", TypeInfo.str(), TypeInfo.str()),
    Method.builtin("startswith", TypeInfo.bool(), TypeInfo.str()),
    Method.builtin("strip", TypeInfo.str(), TypeInfo.str()),
    Method.builtin("upper", TypeInfo.str(), TypeInfo.str()),

    # list
    Method.builtin("append", TypeInfo.none(), TypeInfo.list(), populates_container=True),
    Method.builtin("sort", TypeInfo.none(), TypeInfo.list()),

    # file
    Method.builtin("read", TypeInfo.str(), TypeInfo.textiowraper()),
    Method.builtin("readlines", TypeInfo.list(), TypeInfo.textiowraper()),
)
