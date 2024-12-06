import builtins
import util.objects
import sys
import types


def instanceof_py_function(method_or_function):
    return util.objects.instanceof(
        method_or_function,
        (types.BuiltinFunctionType,
         types.FunctionType,
         types.MethodDescriptorType, type))


def get_py_function_name(method_or_function):
    if instanceof_py_function(method_or_function):
        return method_or_function.__name__
    return None


def get_receiver_type(method_or_function):
    if get_py_function_name(method_or_function) in dir(builtins):
        # builtins, so open/len/write etc, are global (auto imported) and
        # don't have a receiver type in the sense that they are not "called
        # on something"
        return None
    if hasattr(method_or_function, "__objclass__"):
        return getattr(method_or_function, "__objclass__")
    if hasattr(method_or_function, "__module__"):
        modname = method_or_function.__module__
        if modname is not None:
            return get_real_module(sys.modules[modname])
    return None


def get_real_module(module):
    assert isinstance(module, types.ModuleType)
    # check for platform specific module impls and return generic one
    if module.__name__ in ("posixpath", "ntpath"):
        return types.ModuleType("os.path")
    elif module.__name__ in ("posix", "nt"):
        return types.ModuleType("os")
    return module
