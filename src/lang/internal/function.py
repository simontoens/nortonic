import lang.internal.typeinfo as ti
import util.objects as objects


class Function:

    @classmethod
    def get_placeholder(clazz, name):
        return objects.SneakyReadOnly(Function(name))

    def __init__(self, name, rtn_type_infos=None):
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
        # whether this function is defined in the code being processed
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
        # (python) imports required by this function/attribute
        self.imports = []

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
        return not self.has_definition

    @property
    def invocation(self):
        self._reduce_invocations()
        if len(self._invocations) == 0:
            # no invocation was registered
            return None
        return self._invocations[0]

    def clear_registered_arg_type_infos(self):
        self.arg_type_infos = []

    def clear_registered_rtn_type_infos(self):
        self.rtn_type_infos = []

    def register_arg_type_info(self, type_info):
        self.arg_type_infos.append(type_info)

    def register_invocation(self, arg_type_infos):
        if not self.is_builtin:
            self._invocations.append(arg_type_infos)

    def register_rtn_type(self, rtn_type_info):
        assert not self.is_builtin, "register rtn type not supported for builtins"
        assert isinstance(rtn_type_info, ti.TypeInfo)
        self.rtn_type_infos.append(rtn_type_info)

    def get_rtn_type_info(self):
        if len(self.rtn_type_infos) == 0:
            return None
        # allow_none_matches has to be True for methods with multiple return
        # statements, some of them returning NoneType, for example:
        # def foo(i):
        #     if i > 10:
        #         return None
        #     else:
        #         return i
        try:
            return ti.TypeInfo.get_homogeneous_type(self.rtn_type_infos,
                                                    allow_none_matches=True)
        except Exception as e:
            raise Exception("Error while computing return type for function: %s" % self) from e

    def _reduce_invocations(self):
        """
        Function argument types are registered once per function invocation.
        This method only keeps one TypeInfo instance for each argument.
        """
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
                # allow_none_matches is True so that NoneType has the
                # lowest precedence
                type_info = ti.TypeInfo.get_homogeneous_type(type_infos, allow_none_matches=True)
                singleton_invocation.append(type_info)
            assert len(singleton_invocation) == num_args
            self._invocations = [singleton_invocation]

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
                        # we say this function doesn't return multiple values
                        pass
                    else:
                        return True
        return False

    def __repr__(self):
        return "Function %s" % self.name
