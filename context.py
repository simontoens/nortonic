import copy
import scope
import types


class ASTContext:
    
    def __init__(self):
        self._node_to_type_info = {}
        self._function_name_to_function = {}
        self._current_scope = scope.CurrentScope()
        self._ident_names = set()

    @property
    def current_scope(self):
        return self._current_scope

    def register_ident_names(self, ident_names):
        self._ident_names.update(ident_names)

    def get_unqiue_identifier_name(self, preferred_name="t"):
        if not preferred_name in self._ident_names:
            self._ident_names.add(preferred_name)
            return preferred_name
        else:
            counter = 1
            name = "%s%s" % (preferred_name, counter)
            while name in self._ident_names:
                counter += 1
                name = "%s%s" % (preferred_name, counter)
            self._ident_names.add(name)
            return name

    def register_type_info_by_node(self, node, type_info):
        assert type_info is not None, "cannot register None TypeInfo for node %s" % node
        self._node_to_type_info[node] = type_info

    def lookup_type_info_by_node(self, node):
        """
        Returns None if no TypeInfo exists for the given node, otherwise the
        registered TypeInfo instance.
        """
        return self._node_to_type_info.get(node, None)

    def get_type_info_by_node(self, node):
        """
        Returns the associated TypeInfo for the given node, raises if no
        associated TypeInfo exists.
        """
        return self._node_to_type_info[node]

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
                    m_module_name = m.target_instance_type_info.get_metadata(TYPE_INFO_METADATA_MODULE_NAME)
                    target_module_name = target_instance_type_info.get_metadata(TYPE_INFO_METADATA_MODULE_NAME)
                    if m_module_name == target_module_name:
                        # for modules, the attr path has to match
                        return m
                else:
                    return m
        return None

    def get_function(self, function_name, must_exist=False):
        """
        Given a function_name, returns the existing function with that name, or
        creates a new one and returns it, if it doesn't exit yet.

        If must_exist is given as True, then the specified function must exist.
        """
        builtins = self._get_builtin_functions(function_name)
        assert len(builtins) < 2
        if len(builtins) == 1:
            return builtins[0]
        else:
            f = self._function_name_to_function.get(function_name, None)
            if f is None:
                assert not must_exist, "function [%s] does not exist!" % function_name
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
        # whether this function returns a literal
        self.returns_literal = False
        # the docstring, if any
        self.docstring = None
        # whether there's any caller that assigns the function result to a
        # single value: a = foo()
        self._caller_assigns_single_return_value = False
        # whether there's any caller that assigns the function result to
        # multiple values: a, b = foo()
        self._caller_unpacks_return_value = False

    @property
    def caller_assigns_single_return_value(self):
        return self._caller_assigns_single_return_value

    @caller_assigns_single_return_value.setter
    def caller_assigns_single_return_value(self, value):
        if value:
            self._caller_assigns_single_return_value = True

    @property
    def caller_unpacks_return_value(self):
        return self._caller_unpacks_return_value

    @caller_unpacks_return_value.setter
    def caller_unpacks_return_value(self, value):
        if value:
            self._caller_unpacks_return_value = True

    def register_invocation(self, arg_type_infos):
        if not self._is_builtin:
            self.invocations.append(arg_type_infos)

    def register_rtn_type(self, rtn_type_info):
        assert not self._is_builtin, "register rtn type not supported for builtins"
        self.rtn_type_infos.append(rtn_type_info)

    def get_rtn_type_info(self):
        if len(self.rtn_type_infos) == 0:
            return None
        # allow_none_matches has to be True for methods with multiple return
        # statements, some of them returning None, for example:
        # def foo(i):
        #     if i > 10:
        #         return None
        #     else:
        #         return i
        return TypeInfo.get_homogeneous_type(self.rtn_type_infos,
                                             allow_none_matches=True)

    def returns_multiple_values(self, target):
        """
        Whether this function returns a single value or multiple values.
        Note that Python DOES NOT return multiple values, it returns a Tuple
        that may be unpacked at the callsite.
        """
        if target.function_can_return_multiple_values:
            rtn_type_info = self.get_rtn_type_info()
            assert rtn_type_info is not None
            may_return_multiple_values = (rtn_type_info.value_type is tuple and
                                          self.returns_literal)
            if may_return_multiple_values:
                if self._caller_unpacks_return_value:
                    # if all callers unpack values at the callsite, we treat
                    # this function as returning multiple values
                    # a, b = foo()
                    if self._caller_assigns_single_return_value:
                        # ... but if even one caller does this
                        # t = foo()
                        # for simplicity sake (ie to avoid the callsite
                        # ast rewriting)
                        # we say this function doens't return multiple values
                        pass
                    else:
                        return True
        return False

    def __repr__(self):
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



TYPE_INFO_METADATA_MODULE_NAME = "module-name"


class TypeInfo:

    @classmethod
    def none(clazz):
        return TypeInfo(None.__class__)

    @classmethod
    def module(clazz, module_name):
        ti = TypeInfo(types.ModuleType)
        ti.set_metadata(TYPE_INFO_METADATA_MODULE_NAME, module_name)
        return ti
    
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
    def tuple(clazz):
        return TypeInfo(tuple)

    @classmethod
    def textiowraper(clazz):
        import _io
        return TypeInfo(_io.TextIOWrapper)

    @classmethod
    def notype(clazz):
        """
        Noop placeholder type for ast nodes that do not have a type, such
        keywords (like "for")
        """
        class NoType:
            pass
        return TypeInfo(NoType)

    @classmethod
    def late_resolver(clazz, late_resolver):
        return TypeInfo(None, late_resolver=late_resolver)

    @classmethod
    def get_homogeneous_type(clazz, type_infos, allow_none_matches=False):
        """
        Given a list of TypeInfo instances, asserts that they are all the same
        type. Then returns one of them.
        If allow_none_matches is True, then TypeInfo instances that represent
        the None Type are tolerated, but not returned, unless all TypeInfo
        instances represent the None Type.
        """
        assert type_infos is not None
        assert len(type_infos) > 0
        if len(type_infos) == 1:
            return type_infos[0]
        else:
            type_info = None
            for ti in type_infos:
                assert ti is not None
                if type_info is None:
                    type_info = ti
                else:
                    if allow_none_matches:
                        assert ti == type_info or (ti.is_none_type or type_info.is_none_type), "Mismatched types: %s and %s" % (ti, type_info)
                    else:
                        assert ti == type_info, "Mismatched types: %s and %s" % (ti, type_info)
                    if type_info.is_none_type:
                        type_info = ti
            assert type_info is not None
            return type_info

    def __init__(self, value_type, late_resolver=None):
        self.value_type = value_type
        # list of contained types (TypeInfo instances)
        self.contained_type_infos = []
        self.is_pointer = False
        self._metadata = {} # contextual metadata
        # resolves this TypeInfo instance later, based on another TypeInfo
        # this is currently used to relate function argument types with method
        # return types for buildin functions (see sorted/enumerate)
        self._late_resolver = late_resolver

    def set_metadata(self, key, value):
        self._metadata[key] = value

    def get_metadata(self, key):
        return self._metadata.get(key)

    @property
    def is_none_type(self):
        return self.value_type is type(None)

    @property
    def is_sequence(self):
        return self.value_type in (list, tuple)

    @property
    def has_late_resolver(self):
        return self._get_late_resolver() is not None

    @property
    def num_contained_type_infos(self):
        return len(self.contained_type_infos)

    @property
    def contains_homogeneous_types(self):
        if len(self.contained_type_infos) == 0:
            return True
        previous_type_info = None
        for type_infos in self.contained_type_infos:
            for type_info in type_infos:
                if previous_type_info is None:
                    previous_type_info = type_info
                else:
                    if type_info != previous_type_info:
                        return False
        return True

    def register_contained_type(self, index, type_info):
        assert isinstance(type_info, TypeInfo)
        if len(self.contained_type_infos) == index:
            self.contained_type_infos.append([])
        self.contained_type_infos[index].append(type_info)

    def of(self, *type_infos):
        """
        The same as register_contained_type(i, type_info).
        returns self for chaining
        """
        for i, type_info in enumerate(type_infos):
            self.register_contained_type(i, type_info)
        return self

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
        if len(self.contained_type_infos) == 0:
            return ()
        if len(self.contained_type_infos) == 1:
            return (TypeInfo.get_homogeneous_type(self.contained_type_infos[0]),)
        else:
            if is_subscript:
                # for dict[key] syntax, return the value type
                return (TypeInfo.get_homogeneous_type(self.contained_type_infos[1]),)
            ctis = []
            for i in range(0, len(self.contained_type_infos)):
                ctis.append(TypeInfo.get_homogeneous_type(self.contained_type_infos[i]))
            return tuple(ctis)

    def get_value_types(self):
        """
        Returns a tuple of the concrete PY types represented by this TypeInfo
        instance.
        """
        # return a tuple of all contained types, but as value types?
        return (self.value_type,)

    def apply_late_resolver(self, first_arg_type_info):
        if self._late_resolver is not None:
            return self._late_resolver(first_arg_type_info)
        outer_ti = copy.deepcopy(self)
        type_infos_to_process = [outer_ti]
        while len(type_infos_to_process) > 0:
            ti = type_infos_to_process.pop()
            for ct_list in ti.contained_type_infos:
                assert isinstance(ct_list, list)
                for i, ct in enumerate(ct_list):
                    if ct._late_resolver is not None:
                        ct_list[i] = ct._late_resolver(first_arg_type_info)
                    else:
                        type_infos_to_process.append(ct)
        return outer_ti

    def _get_late_resolver(self):
        if self._late_resolver is not None:
            return self._late_resolver
        for ct in self.get_contained_type_infos():
            lr = ct._get_late_resolver()
            if lr is not None:
                return lr
        return None

    def __eq__(self, other):
        if isinstance(other, TypeInfo):
            if self is other:
                return True
            if self.is_pointer != other.is_pointer:
                return False
            if len(self.contained_type_infos) == 0 and len(other.contained_type_infos) == 0:
                return self.value_type is other.value_type
            elif len(self.contained_type_infos) != len(other.contained_type_infos):
                return False
            else:
                self_tis = self.get_contained_type_infos()
                other_tis = other.get_contained_type_infos()
                for i in range(0, len(self_tis)):
                    if self_tis[i] != other_tis[i]:
                        return False
                return True
        else:
            return NotImplemented

    def __hash__(self):
        h = 7
        h = 31 * h + hash(self.value_type)
        h = 31 * h + hash(self.is_pointer)        
        for ti in self.get_contained_type_infos():
            h = 31 * h + hash(ti)
        return h

    def __repr__(self):
        s = "[TypeInfo] %s" % self.value_type
        return s + "(ptr)" if self.is_pointer else s



_BUILTINS = (
    Builtin.function("print", TypeInfo.none()),
    Builtin.function("input", TypeInfo.str()),

    Builtin.function("enumerate",
        TypeInfo.list().of(
            TypeInfo.tuple().of(
                TypeInfo.int(),
                TypeInfo.late_resolver(lambda ati: ati.get_contained_type_info_at(0))))),
    Builtin.function("range", TypeInfo.list().of(TypeInfo.int())),
    Builtin.function("len", TypeInfo.int()),
    Builtin.function("open", TypeInfo.textiowraper()),
    Builtin.function("sorted", TypeInfo.late_resolver(lambda ati: ati)),

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
