import ast
import copy
import nodeattrs
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
        ti = self._node_to_type_info.get(node)
        assert ti is not None, "Cannot get TypeInfo for node: %s %s" % (id(node), ast.dump(node))
        return ti

    def has_type_info(self, node):
        return node in self._node_to_type_info

    def clear_type_infos(self):
        self._node_to_type_info = {}

    def clear_functions(self):
        nodeattrs.remove_functions_from_nodes()
        self._function_name_to_function = {}

    def clear_all(self):
        self.clear_type_infos()
        self.clear_functions()

    def get_method(self, method_name, target_instance_type_info):
        """
        Returns a pre-registered method.
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

        # once we have re-written methods (append -> add for ex), we cannot
        # find the pre-registered methods anymore - that's ok because we are
        # tracking their return type differently
        # in order to keep the code path saner, we just return a method
        # instance for anything we don't recognize - more examples:
        # "put", "get", "substring", "length", "size", "equals",
        # "startsWith", "endsWith", "trim", "toString", "indexOf",
        # "toLowerCase", "toPath", "split"): # trying things

        # this is questionable and unclear - would be good to understand
        # why this is needed once we support user defined mehtod (aka classes)
        if method_name in self._function_name_to_function:
            return self._function_name_to_function[method_name]
        m = Function(method_name)
        m.target_instance_type_info = target_instance_type_info
        self._function_name_to_function[method_name] = m
        return m

    def get_function(self, function_name, must_exist=False):
        """
        Given a function_name, returns the existing function with that name, or
        creates a new one and returns it, if it doesn't exit yet.

        If must_exist is True, then the specified function must exist.
        """
        builtins = self._get_builtin_functions(function_name)
        assert len(builtins) < 2
        if len(builtins) == 1:
            f = builtins[0]
            if f.target_instance_type_info is None:
                # if target_instance_type_info is not None, this is a method
                # called on an instance, so don't return it as a function
                return f
        f = self._function_name_to_function.get(function_name, None)
        if f is None:
            assert not must_exist, "function [%s] does not exist!" % function_name
            f = Function(function_name)
            self._function_name_to_function[function_name] = f
        return f

    def get_user_functions(self):
        return tuple([f for f in self._function_name_to_function.values() if not f._is_builtin and f.has_definition])

    def _get_builtin_functions(self, name):
        return [f for f in _BUILTINS if f.name == name]


class Function:

    def __init__(self, name, rtn_type_infos=None, is_builtin=False):
        assert name is not None
        # the name of this function
        self.name = name
        # list of tuples of TypeInfos for each arg in positional order - one
        # for each invocation
        self._invocations = []
        # list of return types as TypeInfos, one for each return stmt
        self.rtn_type_infos = [] if rtn_type_infos is None else rtn_type_infos
        # for methods, the type the method is called on: l.append -> list
        # for functions, the module that "owns" the method: os.mkdir -> os
        self.target_instance_type_info = None
        # builtin function/method (not defined in code being processed)
        self._is_builtin = is_builtin
        # whether this function is defined in the code being processed
        # FIXME TODO this should replace is_builtin
        self.has_definition = False
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
        # the functions argument types (positional only supported currently)
        # these are NOT always the same as the invocation argument types because
        # of pointers!
        self.arg_type_infos = []

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

    @property
    def is_builtin(self):
        if self._is_builtin:
            return True
        if len(self.rtn_type_infos) == 0:
            # if we didn't process the function definition, we never registered
            # a return type - we can use this a proxy to determine whether
            # a function instance is built-in
            return True
        return False

    @property
    def invocation(self):
        if len(self._invocations) == 0:
            # no invocation was registered
            return None
        #self._reduce_arg_type_infos()
        return self._invocations[0]

    def clear_registered_arg_type_infos(self):
        self.arg_type_infos = []

    def register_arg_type_info(self, type_info):
        self.arg_type_infos.append(type_info)

    def register_invocation(self, arg_type_infos):
        if not self._is_builtin:
            self._invocations.append(arg_type_infos)

    def register_rtn_type(self, rtn_type_info):
        assert not self._is_builtin, "register rtn type not supported for builtins"
        assert isinstance(rtn_type_info, TypeInfo)
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

    def reduce_type_infos(self):
        """
        Function argument types are registered once per function invocation.
        This method only keeps one TypeInfo instance for each argument.

        For each of this function's return statements, a TypeInfo instance is
        registered. This method keeps only one of them.
        """
        # method arguments
        self._reduce_arg_type_infos()

        # method return type
        ti = self.get_rtn_type_info() # raises if type mismatch is enountered
        if ti is not None:
            self.rtn_type_infos = [ti]

    def _reduce_arg_type_infos(self):
        if len(self._invocations) > 1:
            singleton_invocation = []
            # sanity
            num_args = None
            for invocation in self._invocations:
                if num_args is None:
                    num_args = len(invocation)
                else:
                    assert len(invocation) == num_args
            # determine single arg type
            for arg_pos in range(0, num_args):
                type_infos = [invoc[arg_pos] for invoc in self._invocations]
                # raises if type mismatch is enountered
                type_info = TypeInfo.get_homogeneous_type(type_infos)
                singleton_invocation.append(type_info)
            assert len(singleton_invocation) == num_args
            self._invocations = [singleton_invocation]

    def replace_arg_type_info_at(self, position, new_arg_type_info):
        assert len(self._invocations) > 0
        invocation = self._invocations[0]
        assert len(invocation) > position
        invocation[position] = new_arg_type_info

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


class ContainerMetadata:

    def __init__(self, transformer):
        self._transformer = transformer

    def update_transfomer(self, transformer):
        self._transformer = transformer
    

class ListContainerMetadata(ContainerMetadata):

    def __init__(self):
        super().__init__(lambda target, value: (target, value))

    def register(self, *args):
        target_ti, value_ti = self._transformer(*args)
        assert target_ti.is_container
        target_ti.register_contained_type(0, value_ti)


class DictContainerMetadata(ContainerMetadata):

    def __init__(self):
        super().__init__(lambda target, key, value: (target, key, value))

    def register(self, *args):
        target_ti, key_ti, value_ti = self._transformer(*args)
        assert target_ti.is_container
        target_ti.register_contained_type(0, key_ti)
        target_ti.register_contained_type(1, value_ti)

    
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
    def method(clazz, name, rtn_type_info, target_instance_type_info):
        assert rtn_type_info is not None
        assert target_instance_type_info is not None
        f = Function(name, (rtn_type_info,), is_builtin=True)
        f.target_instance_type_info = target_instance_type_info
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
        class LateResolverType:
            pass
        return TypeInfo(LateResolverType, late_resolver=late_resolver)

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
                        assert ti == type_info or (ti.is_none_type or type_info.is_none_type), "Mismatched types: %s [%s] and %s [%s]" % (ti, ti.contained_type_infos, type_info, type_info.contained_type_infos)
                    else:
                        assert ti == type_info, "Mismatched types: %s %s and %s %s" % (ti, ti.contained_type_infos, type_info, type_info.contained_type_infos)
                    if type_info.is_none_type:
                        type_info = ti
            assert type_info is not None
            return type_info

    def __init__(self, value_type, late_resolver=None):
        assert value_type is not None
        assert not isinstance(value_type, TypeInfo)
        self.value_type = value_type
        # list of contained types (TypeInfo instances)
        self.contained_type_infos = []
        self.is_pointer = False
        self._metadata = {} # contextual metadata
        # resolves this TypeInfo instance later, based on another TypeInfo
        # this is currently used to relate function argument types with method
        # return types for buildin functions (see sorted/enumerate)
        self._late_resolver = late_resolver

        self.backing_type_info = None

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
    def is_container(self):
        return self.is_sequence or self.value_type is dict

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
        assert isinstance(type_info, TypeInfo), "expected TypeInfo but got %s" % type_info
        if len(self.contained_type_infos) == index:
            self.contained_type_infos.append([])
        self.contained_type_infos[index].append(type_info)
        if self.backing_type_info is not None:
            self.backing_type_info.register_contained_type(index, type_info)

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

    def propagate_contained_type_infos(self, type_info):
        """
        Adds the contained type infos of the specified type_info to this
        instance's contained type infos.
        """
        for i, ti in enumerate(type_info.get_contained_type_infos()):
            self.register_contained_type(i, ti)

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


    DEEP_COPY_ENABLED = True

    def __deepcopy__(self, d):
        if TypeInfo.DEEP_COPY_ENABLED:
            clone = TypeInfo(self.value_type, self.late_resolver)
            for attr in self.__dict__:
                value = copy.deepcopy(self.__dict__[attr], d)
                setattr(clone, attr, value)
            return clone
        else:
            return self

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


# this needs to move out of here
PRINT_BUILTIN  = Builtin.function("print", TypeInfo.none())
LEN_BUILTIN = Builtin.function("len", TypeInfo.int())
STR_BUILTIN = Builtin.function("str", TypeInfo.str())
SPLIT_BUILTIN = Builtin.method("split", TypeInfo.list().of(TypeInfo.str()), TypeInfo.str())

_BUILTINS = (
    # global
    Builtin.function("input", TypeInfo.str()),

    Builtin.function("enumerate",
        TypeInfo.list().of(
            TypeInfo.tuple().of(
                TypeInfo.int(),
                TypeInfo.late_resolver(lambda ati: ati.get_contained_type_info_at(0))))),
    Builtin.function("range", TypeInfo.list().of(TypeInfo.int())),
    Builtin.function("open", TypeInfo.textiowraper()),
    Builtin.function("sorted", TypeInfo.late_resolver(lambda ati: ati)),
    LEN_BUILTIN,
    PRINT_BUILTIN,
    STR_BUILTIN,

    # str
    Builtin.method("find", TypeInfo.int(), TypeInfo.str()),
    Builtin.method("index", TypeInfo.int(), TypeInfo.str()),
    Builtin.method("endswith", TypeInfo.bool(), TypeInfo.str()),
    Builtin.method("join", TypeInfo.str(), TypeInfo.str()),
    Builtin.method("lower", TypeInfo.str(), TypeInfo.str()),
    Builtin.method("startswith", TypeInfo.bool(), TypeInfo.str()),
    SPLIT_BUILTIN,
    Builtin.method("strip", TypeInfo.str(), TypeInfo.str()),
    Builtin.method("upper", TypeInfo.str(), TypeInfo.str()),

    # list
    Builtin.method("append", TypeInfo.none(), TypeInfo.list()),
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
