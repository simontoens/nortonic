import copy
import types


class TypeInfo:

    # special modes:

    # when copying object graphs using copy.deepcopy, optionally do not deep
    # copy the referenced TypeInfo instances
    DEEP_COPY_ENABLED = True

    # when comparing types, by default the "pointerness" of a type is part of
    # the equality check, meaning *string == *string and *string != string
    # this can be relaxed so that *string == string - this less-strict mode
    # is necessary when transitioning the ast from no pointer types to pointers
    # for some types
    TYPE_EQUALITY_CHECK_INCLUDES_POINTERS = True


    @classmethod
    def none(clazz):
        return TypeInfo(None.__class__)

    @classmethod
    def module(clazz, module_name):
        ti = TypeInfo(types.ModuleType)
        ti.module_name = module_name
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
    def function(clazz, function):
        ti = TypeInfo(types.FunctionType)
        ti.function = function
        return ti

    @classmethod
    def clazz(clazz, name):
        ti = TypeInfo(type)
        # TODO
        ti.module_name = name
        return ti

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
        ti = TypeInfo(NoType)
        ti.is_real = False
        return ti

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
                        if TypeInfo._are_type_infos_equal(type_info, ti, True):
                            pass
                        else:
                            raise AssertionError("Mismatched types: %s [%s] and %s [%s]" % (ti, ti.contained_type_infos, type_info, type_info.contained_type_infos))
                    else:
                        if TypeInfo._are_type_infos_equal(type_info, ti, False):
                            pass
                        else:
                            raise AssertionError("Mismatched types: %s %s and %s %s" % (ti, ti.contained_type_infos, type_info, type_info.contained_type_infos))
                    if type_info.is_none_type:
                        type_info = ti
            assert type_info is not None
            return type_info

    @classmethod
    def _are_type_infos_equal(clazz, ti1, ti2, allow_none_matches=False):
        if ti1 is ti2:
            return True
        if ti1 == ti2:
            return True
        else:
            if allow_none_matches:
                if ti1.is_none_type or ti2.is_none_type:
                    return True
            if ti1.value_type == ti2.value_type:
                return not TypeInfo.TYPE_EQUALITY_CHECK_INCLUDES_POINTERS
            else:
                return False

    def __init__(self, value_type, late_resolver=None):
        assert value_type is not None
        assert not isinstance(value_type, TypeInfo)
        self.value_type = value_type
        # list of contained types (TypeInfo instances)
        self.contained_type_infos = []
        self.is_pointer = False
        # resolves this TypeInfo instance later, based on another TypeInfo
        # this is currently used to relate function argument types with method
        # return types for buildin functions (see sorted/enumerate)
        self._late_resolver = late_resolver
        # TODO add comment
        self.backing_type_info = None
        # function types (lambda) carry their function metadata
        self.function = None
        # whether this is a real type, or the placeholder no-type type
        self.is_real = True
        # for module types only, the module name
        self.module_name = None
        # types have methods
        self.methods = []

        # if value_type is str:
        #     self.methods.append(builtins.STRIP)

    @property
    def is_none_type(self):
        return self.value_type is type(None)

    @property
    def is_function(self):
        return self.value_type is types.FunctionType

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
        if type_info.is_none_type:
            # silently skipping this isn't great, but it allows us to
            # centralize this check instead of adding it in various places
            # before calling this method
            # the type can be NoneType here if it is the result of a method
            # that returns None - this is a valid return type, but doesn't
            # tell us anything about the contained type:
            # def foo(d):
            #     return None
            # d = {}
            # d["k1"] = foo()
            # d["k2"] = "val2"
            pass
        else:
            for i in range(len(self.contained_type_infos), index+1):
                self.contained_type_infos.append([])
            self.contained_type_infos[index].append(type_info)
            if self.backing_type_info is not None:
                 self.backing_type_info.register_contained_type(index, type_info)

    def of(self, *type_infos):
        """
        The same as register_contained_type(i, type_info).
        returns self for chaining
        """
        for i, ti in enumerate(type_infos):
            if not isinstance(ti, TypeInfo):
                ti = TypeInfo(ti)
            self.register_contained_type(i, ti)
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
                if len(self.contained_type_infos[i]) == 0:
                    # this can happen when another (higher slot) has type
                    # info(s) but the lower slot does not yet
                    # in this case we have None as a marker because this is
                    # an intermediary state
                    # for example, a dict's value type may already be known
                    # but the key type isn't
                    ctis.append(None)
                else:
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
            if ct is not None:
                lr = ct._get_late_resolver()
                if lr is not None:
                    return lr
        return None

    def is_equal_ignoring_pointers(self, other):
        return self._is_equal(other, consider_pointers=False)

    def __eq__(self, other):
        return self._is_equal(other, consider_pointers=True)

    def _is_equal(self, other, consider_pointers):
        if isinstance(other, TypeInfo):
            if self is other:
                return True
            if self.value_type is not other.value_type:
                return False
            if self.module_name != other.module_name:
                return False
            if consider_pointers:
                if self.is_pointer != other.is_pointer:
                    return False
            if self.module_name != other.module_name:
                return False
            if len(self.contained_type_infos) != len(other.contained_type_infos):
                return False
            else:
                self_tis = self.get_contained_type_infos()
                other_tis = other.get_contained_type_infos()
                for i in range(0, len(self_tis)):
                    if not self_tis[i]._is_equal(other_tis[i], consider_pointers):
                        return False
                return True
        else:
            return NotImplemented

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
        h = 31 * h + hash(self.module_name)
        h = 31 * h + hash(self.is_pointer)
        for ti in self.get_contained_type_infos():
            h = 31 * h + hash(ti)
        return h

    def __repr__(self):
        s = "[TypeInfo] %s" % self.value_type
        s = s + "(ptr)" if self.is_pointer else s
        if self.value_type is types.ModuleType:
            s += " " + self.module_name
        return s
