import util.objects
import types


def instanceof_py_function(method_or_function):
    return util.objects.instanceof(
        method_or_function,
        (types.BuiltinFunctionType, types.MethodDescriptorType, type))


def get_py_function_name(method_or_function):
    if instanceof_py_function(method_or_function):
        return method_or_function.__name__
    return None


def get_receiver_type(method_or_function):
    if hasattr(method_or_function, "__objclass__"):
        return getattr(method_or_function, "__objclass__")
    return None
