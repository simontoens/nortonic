import lang.internal.function as function
import lang.internal.typeinfo as ti
import lang.nodebuilder as nodebuilder
import visitor.nodeattrs as nodeattrs


class Builtin:
    """
    This class has factory methods for Function instances.
    """
    @classmethod
    def function(clazz, name, rtn_type_info, module=None, imports=[]):
        assert rtn_type_info is not None
        f = function.Function(name, (rtn_type_info,))
        f.target_instance_type_info = module
        f.imports = imports
        return f

    @classmethod
    def method(clazz, name, rtn_type_info, target_instance_type_info, imports=[]):
        assert rtn_type_info is not None
        assert target_instance_type_info is not None
        f = function.Function(name, (rtn_type_info,))
        f.target_instance_type_info = target_instance_type_info
        f.imports = imports
        return f

    @classmethod
    def attribute(clazz, name, type_info, module):
        assert type_info is not None
        f = function.Function(name, (type_info,))
        f.target_instance_type_info = module
        return f


PRINT  = Builtin.function("print", ti.TypeInfo.none())
LEN = Builtin.function("len", ti.TypeInfo.int())
STR = Builtin.function("str", ti.TypeInfo.str())
SPLIT = Builtin.method("split", ti.TypeInfo.list().of(ti.TypeInfo.str()), ti.TypeInfo.str())
STRIP = Builtin.method("strip", ti.TypeInfo.str(), ti.TypeInfo.str())
STARTSWITH = Builtin.method("startswith", ti.TypeInfo.bool(), ti.TypeInfo.str())
ENDSWITH = Builtin.method("endswith", ti.TypeInfo.bool(), ti.TypeInfo.str())


ALL = (
    # global
    Builtin.function("input", ti.TypeInfo.str()),

    Builtin.function("enumerate",
        ti.TypeInfo.list().of(
            ti.TypeInfo.tuple().of(
                ti.TypeInfo.int(),
                ti.TypeInfo.late_resolver(lambda ati: ati.get_contained_type_info_at(0))))),
    Builtin.function("range", ti.TypeInfo.list().of(ti.TypeInfo.int())),
    Builtin.function("open", ti.TypeInfo.textiowraper()),
    Builtin.function("sorted", ti.TypeInfo.late_resolver(lambda ati: ati)),
    LEN,
    PRINT,
    STR,
    # str
    Builtin.method("find", ti.TypeInfo.int(), ti.TypeInfo.str()),
    Builtin.method("index", ti.TypeInfo.int(), ti.TypeInfo.str()),
    Builtin.method("join", ti.TypeInfo.str(), ti.TypeInfo.str()),
    Builtin.method("lower", ti.TypeInfo.str(), ti.TypeInfo.str()),
    STARTSWITH,
    ENDSWITH,
    SPLIT,
    STRIP,

    Builtin.method("upper", ti.TypeInfo.str(), ti.TypeInfo.str()),

    # list
    Builtin.method("append", ti.TypeInfo.none(), ti.TypeInfo.list()),
    Builtin.method("sort", ti.TypeInfo.none(), ti.TypeInfo.list()),

    # file
    Builtin.method("read", ti.TypeInfo.str(), ti.TypeInfo.textiowraper()),
    Builtin.method("readlines", ti.TypeInfo.list().of(ti.TypeInfo.str()), ti.TypeInfo.textiowraper()),
    Builtin.method("write", ti.TypeInfo.none(), ti.TypeInfo.textiowraper()),

    # os    
    Builtin.attribute("sep", ti.TypeInfo.str(), ti.TypeInfo.module("os")),
    Builtin.attribute("path", ti.TypeInfo.module("os.path"), ti.TypeInfo.module("os")),

    # os.path
    Builtin.method("join", ti.TypeInfo.str(), ti.TypeInfo.module("os.path"), imports="os"),
    Builtin.attribute("sep", ti.TypeInfo.str(), ti.TypeInfo.module("os.path")),
)


def get_globals():
    """
    Returns global function identifiers attached to fake AST nodes.
    """
    nodes = []
    for func in ALL:
        if func.target_instance_type_info is None:
            n = nodebuilder.funcdef(func.name)
            nodeattrs.set_function(n, func)
            nodes.append(n)
    return tuple(nodes)
