from lang.internal import Function, TypeInfo


class Builtin:
    """
    This class has factory methods for Function instances.
    """

    @classmethod
    def function(clazz, name, rtn_type_info, module=None, imports=[]):
        assert rtn_type_info is not None
        f = Function(name, (rtn_type_info,), is_builtin=True)
        f.target_instance_type_info = module
        f.imports = imports
        return f

    @classmethod
    def method(clazz, name, rtn_type_info, target_instance_type_info, imports=[]):
        assert rtn_type_info is not None
        assert target_instance_type_info is not None
        f = Function(name, (rtn_type_info,), is_builtin=True)
        f.target_instance_type_info = target_instance_type_info
        f.imports = imports
        return f

    @classmethod
    def attribute(clazz, name, type_info, module):
        assert type_info is not None
        f = Function(name, (type_info,), is_builtin=True)
        f.target_instance_type_info = module
        return f


PRINT  = Builtin.function("print", TypeInfo.none())
LEN = Builtin.function("len", TypeInfo.int())
STR = Builtin.function("str", TypeInfo.str())
SPLIT = Builtin.method("split", TypeInfo.list().of(TypeInfo.str()), TypeInfo.str())
STRIP = Builtin.method("strip", TypeInfo.str(), TypeInfo.str())
STARTSWITH = Builtin.method("startswith", TypeInfo.bool(), TypeInfo.str())
ENDSWITH = Builtin.method("endswith", TypeInfo.bool(), TypeInfo.str())


BUILTINS = (
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
    LEN,
    PRINT,
    STR,
    # str
    Builtin.method("find", TypeInfo.int(), TypeInfo.str()),
    Builtin.method("index", TypeInfo.int(), TypeInfo.str()),
    Builtin.method("join", TypeInfo.str(), TypeInfo.str()),
    Builtin.method("lower", TypeInfo.str(), TypeInfo.str()),
    STARTSWITH,
    ENDSWITH,
    SPLIT,
    STRIP,

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
    Builtin.method("join", TypeInfo.str(), TypeInfo.module("os.path"), imports="os"),
    Builtin.attribute("sep", TypeInfo.str(), TypeInfo.module("os.path")),
)
