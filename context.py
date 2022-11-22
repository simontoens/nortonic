import ast
import scope
import types


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
        """
        Returns a pre-registered method.  Once we support user-defined methods
        (classes), this method should get merged with get_function below.
        """
        assert method_name is not None
        assert target_instance_type_info is not None
        methods = self._get_builtin_functions(method_name)
        for m in methods:
            if target_instance_type_info.value_type is m.target_instance_type_info.value_type:
                if target_instance_type_info.value_type is types.ModuleType:
                    if target_instance_type_info.metadata == m.target_instance_type_info.metadata:
                        # for modules, the attr path has to match - it is stored
                        # as "metadata"
                        return m
                else:
                    return m
        return None

    def get_function(self, function_name):
        """
        Gets existing, or creates new Function instance for the specified
        function_name.
        """
        builtins = self._get_builtin_functions(function_name)
        assert len(builtins) < 2
        if len(builtins) == 1:
            return builtins[0]
        else:
            f = self._function_name_to_function.get(function_name, None)
            if f is None:
                f = Function(function_name)
                self._function_name_to_function[function_name] = f
            return f

    def _get_builtin_functions(self, name):
        return [f for f in _BUILTINS if f.name == name]


class Function:

    def __init__(self, name, rtn_type_infos=None, is_builtin=False):
        assert name is not None
        self.name = name
        # list of tuples of TypeInfos for each arg in positional order - one
        # for each invocation
        self.invocations = []
        # list of return types as TypeInfos, one for each return stmt
        self.rtn_type_infos = [] if rtn_type_infos is None else rtn_type_infos
        # for methods, the type the method is called on: l.append -> list
        # for functions, the module that "owns" the method: os.mkdir -> os
        self.target_instance_type_info = None
        # if the target instance is a container, whether this method adds to it
        self.populates_target_instance_container = False
        # builtin function/method?
        self._is_builtin = is_builtin
        # whether this function has explicit return statement(s)
        self.has_explicit_return = False
        # the docstring, if any
        self.docstring = None

    def register_invocation(self, arg_type_infos):
        if not self._is_builtin:
            self.invocations.append(arg_type_infos)

    def register_rtn_type(self, rtn_type_info):
        assert not self._is_builtin, "register rtn type not supported for builtins"
        self.rtn_type_infos.append(rtn_type_info)

    def get_rtn_type_info(self):
        return TypeInfo.find_significant(self.rtn_type_infos)

    def __str__(self):
        return "Function %s" % self.name


class Builtin:
    """
    This class has factory methods for Function instances.
    """

    @classmethod
    def function(clazz, name, rtn_type_info, module=None):
        assert rtn_type_info is not None
        f = Function(name, (rtn_type_info,), is_builtin=True)
        f.target_instance_type_info = module
        return f

    @classmethod
    def method(clazz, name, rtn_type_info, target_instance_type_info,
               populates_container=False):
        assert rtn_type_info is not None
        assert target_instance_type_info is not None
        f = Function(name, (rtn_type_info,), is_builtin=True)
        f.target_instance_type_info = target_instance_type_info
        f.populates_target_instance_container = populates_container
        return f

    @classmethod
    def attribute(clazz, name, type_info, module):
        assert type_info is not None
        f = Function(name, (type_info,), is_builtin=True)
        f.target_instance_type_info = module
        return f


class TypeInfo:

    @classmethod
    def none(clazz):
        return TypeInfo(None.__class__)

    @classmethod
    def module(clazz, module_name):
        return TypeInfo(types.ModuleType, metadata=module_name)
    
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

    @classmethod
    def find_significant(clazz, type_infos):
        """
        Looks through the given list of type_infos and returns:
          - None if len(type_infos) == 0
          - None if all type_infos are None
          - If a single instance in type_infos is not None, returns that
          - If multiple instances are not None, prefers any instance that does
          - not have a value_type of NoneType (so prefers int/str etc over
            NoneType)
        """
        assert type_infos is not None
        if len(type_infos) == 0:
            return None
        candidate_type_info = None
        for ti in type_infos:
            if ti is not None:
                candidate_type_info = ti
                if not ti.is_none_type:
                    return ti
        return candidate_type_info

    def __init__(self, value_type, metadata=None):
        self.value_type = value_type
        self.contained_type_infos = None # list of contained types
        self.metadata = metadata # arbitrary metadata

    @property
    def is_none_type(self):
        return self.value_type is type(None)

    @property
    def is_sequence(self):
        return self.value_type in (list, tuple)

    def register_contained_type(self, index, type_info):
        if self.contained_type_infos is None:
            self.contained_type_infos = []
        if len(self.contained_type_infos) == index:
            self.contained_type_infos.append([])
        self.contained_type_infos[index].append(type_info)

    def of(self, type_info):
        """
        The same as register_contained_type(0, type_info)
        returns self for chaining
        """
        self.register_contained_type(0, type_info)
        return self

    def get_contained_type_info(self, is_subscript=False):
        """
        Returns the contained type(s) as a (composite) TypeInfo instance.

        is_subscript is a hack to deal with dict[] syntax - this needs to be
        generalized.
        """
        ctis = self.get_contained_type_infos(is_subscript)
        if len(ctis) == 0:
            return None
        if len(ctis) == 1:
            return ctis[0]
        return CompositeTypeInfo(ctis)

    def get_contained_type_info_at(self, index, assume_homogeneous_types=True):
        """
        Returns the contained type at the specified index.
        """
        ctis = self.get_contained_type_infos(is_subscript=False)
        if len(ctis) == 0:
            return None
        if index < len(ctis):
            return ctis[index]
        if assume_homogeneous_types:
            # we don't have a type info at the specified index, let's hope
            # for the best though
            return ctis[0]
        assert False, "no type info at index %s" % index

    def get_contained_type_infos(self, is_subscript=False):
        """
        Returns the contained type(s) as an interable of TypeInfo instances.

        is_subscript is a hack to deal with dict[] syntax - this needs to be
        generalized.
        """
        if self.contained_type_infos is None:
            return ()
        assert len(self.contained_type_infos) > 0
        if len(self.contained_type_infos) == 1:
            return (TypeInfo.find_significant(self.contained_type_infos[0]),)
        else:
            if is_subscript:
                # for dict[key] syntax, return the value type
                return (TypeInfo.find_significant(self.contained_type_infos[1]),)
            ctis = []
            for i in range(0, len(self.contained_type_infos)):
                ctis.append(TypeInfo.find_significant(self.contained_type_infos[i]))
            return tuple(ctis)

    def get_value_types(self):
        """
        Returns a tuple of the concrete PY types represented by this TypeInfo
        instance.
        """
        # return a tuple of all contained types, but as value types?
        return (self.value_type,)

    def __repr__(self):
        return str("[TypeInfo] %s" % self.value_type)

    __str__ = __repr__


class CompositeTypeInfo:

    def __init__(self, type_infos):
        self.type_infos = tuple(type_infos)

    def get_contained_type_infos(self):
        return self.type_infos

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
    Builtin.function("print", TypeInfo.none()),
    Builtin.function("input", TypeInfo.str()),

    Builtin.function("len", TypeInfo.int()),
    Builtin.function("open", TypeInfo.textiowraper()),
    Builtin.function("sorted", TypeInfo.none()),#TODO rtn type is based on 1 arg

    # str
    Builtin.method("find", TypeInfo.int(), TypeInfo.str()),
    Builtin.method("index", TypeInfo.int(), TypeInfo.str()),
    
    Builtin.method("endswith", TypeInfo.bool(), TypeInfo.str()),
    Builtin.method("join", TypeInfo.str(), TypeInfo.str()),
    Builtin.method("lower", TypeInfo.str(), TypeInfo.str()),
    Builtin.method("startswith", TypeInfo.bool(), TypeInfo.str()),
    Builtin.method("split", TypeInfo.list().of(TypeInfo.str()), TypeInfo.str()),
    Builtin.method("strip", TypeInfo.str(), TypeInfo.str()),
    Builtin.method("upper", TypeInfo.str(), TypeInfo.str()),

    # list
    Builtin.method("append", TypeInfo.none(), TypeInfo.list(), populates_container=True),
    Builtin.method("sort", TypeInfo.none(), TypeInfo.list()),

    # file
    Builtin.method("read", TypeInfo.str(), TypeInfo.textiowraper()),
    Builtin.method("readlines", TypeInfo.list().of(TypeInfo.str()), TypeInfo.textiowraper()),
    Builtin.method("write", TypeInfo.none(), TypeInfo.textiowraper()),

    # os    
    Builtin.attribute("sep", TypeInfo.str(), TypeInfo.module("os")),
    Builtin.attribute("path", TypeInfo.module("os.path"), TypeInfo.module("os")),

    # os.path
    Builtin.method("join", TypeInfo.str(), TypeInfo.module("os.path")),
    Builtin.attribute("sep", TypeInfo.str(), TypeInfo.module("os.path")),
)
