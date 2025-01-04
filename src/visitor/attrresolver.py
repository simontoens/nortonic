import collections
import lang.internal.function as function
import lang.internal.typeinfo as typeinfo
import types
import util.objects as objects


NO_MODULE = typeinfo.TypeInfo.module("")


class AttributeResolver:

    def __init__(self, default_resolver=None):
        self._default_resolver = default_resolver
        # type_info -> dict[attr name->attr value]
        self._attributes = collections.defaultdict(dict)

    def register(self, type_info, value_or_name, value=None):
        assert isinstance(type_info, typeinfo.TypeInfo)
        assert value_or_name is not None
        if value is None:
            assert objects.instanceof(value_or_name, function.Function)
            name = value_or_name.name
            value = value_or_name
        else:
            name = value_or_name
        assert objects.instanceof(value, (function.Function, typeinfo.TypeInfo))
        if type_info.name is not None:
            self._register_modules(type_info.name)
        self._register(type_info, name, value)

    def resolve_to_type(self, receiver_type_info, attr_name):
        resolved = self._resolve(receiver_type_info, attr_name)
        if resolved is not None:
            objects.assert_isinstance(resolved, (typeinfo.TypeInfo, function.Function))
            if objects.instanceof(resolved, typeinfo.TypeInfo):
                return resolved
            elif objects.instanceof(resolved, function.Function):
                return resolved.get_rtn_type_info()
        if self._default_resolver is not None:
            return self._default_resolver.resolve_to_type(receiver_type_info, attr_name)
        return None

    def resolve_to_function(self, receiver_type_info, attr_name):
        resolved = self._resolve(receiver_type_info, attr_name)
        if resolved is not None:
            objects.assert_isinstance(resolved, function.Function)
            return resolved
        if self._default_resolver is not None:
            return self._default_resolver.resolve_to_function(receiver_type_info, attr_name)
        return None

    def get_all_attributes_name_and_type(self, receiver_type):
        name_and_type = []
        for name, value in self._get_attributes(receiver_type).items():
            if objects.instanceof(value, function.Function):
                pass
            else:
                name_and_type.append((name, value))
        return name_and_type

    def get_top_level_functions(self):
        top_level_functions = []
        for name, value in self._get_attributes(NO_MODULE).items():
            if objects.instanceof(value, function.Function):
                top_level_functions.append(value)
        if self._default_resolver is not None:
            top_level_functions += self._default_resolver.get_top_level_functions()
        return top_level_functions

    def _register_modules(self, attr_path):
        """
        Registers intermediary modules, for example:
        os.path -> module(os) has attr "path" of value module("os.path")
        """
        base = None
        attributes = attr_path.split(".")
        for attr in attributes:
            if base is None:
                base = attr
            else:
                fq_path = base + "." + attr
                module = typeinfo.TypeInfo.module(fq_path)
                self._register(typeinfo.TypeInfo.module(base), attr, module)
                base = fq_path

    def _register(self, type_info, name, value):
        key = _build_key(type_info)
        self._attributes[key][name] = value

    def _resolve(self, receiver_type_info, attr_name):
        return self._get_attributes(receiver_type_info).get(attr_name)

    def _get_attributes(self, type_info):
        return self._attributes.get(_build_key(type_info), dict())

def _build_key(type_info):
    # using the type_info instance directly as key is problematic because the
    # contained type infos are not stable during the analysis phase
    # for example list vs list[string]
    # the hash/equality considers contained type infos currently
    return "%s%s" % (type_info.value_type, "" if type_info.name is None else "_%s" % type_info.name)
    
