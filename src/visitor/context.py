from lang import internal
import ast
import lang.builtins as builtins
import lang.scope as scope
import types
import visitor.nodeattrs as nodeattrs


class ASTContext:
    
    def __init__(self):
        self._node_to_type_info = {}
        self._function_name_to_function = {}
        self._current_scope = scope.CurrentScope()
        self._import_names = set()
        self._ident_names = set()

    @property
    def current_scope(self):
        return self._current_scope

    def register_imports(self, import_names):
        if isinstance(import_names, str):
            self._import_names.add(import_names)
        else:
            if not isinstance(import_names, set):
                import_names = set(import_names)
                self._import_names.update(import_names)

    def get_imports(self):
        return sorted(self._import_names)

    def register_ident_names(self, ident_names):
        self._ident_names.update(ident_names)

    def get_unique_identifier_name(self, name_prefix=None):
        preferred_name = "t" if name_prefix is None else name_prefix
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
        assert isinstance(node, ast.AST)
        assert isinstance(type_info, internal.TypeInfo), "expected TypeInfo for node %s but got %s" % (node, type_info)
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

    def associate_type_info_with_other_node(self, src_node, dest_node):
        ti = self.lookup_type_info_by_node(src_node)
        if ti is not None:
            self.register_type_info_by_node(dest_node, ti)

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
                    m_module_name = m.target_instance_type_info.module_name
                    target_module_name = target_instance_type_info.module_name
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
        # why this is needed once we support user defined methods (aka classes)
        if method_name in self._function_name_to_function:
            return self._function_name_to_function[method_name]
        m = internal.Function(method_name)
        m.target_instance_type_info = target_instance_type_info
        self._function_name_to_function[method_name] = m
        return m

    def _get_builtin_functions(self, name):
        return [f for f in builtins.ALL if f.name == name]


class ContainerMetadata:

    def __init__(self, transformer):
        self._transformer = transformer

    def update_transformer(self, transformer):
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
        """
        None's may be passed in, they will be skipped.
        """
        target_ti, key_ti, value_ti = self._transformer(*args)
        assert target_ti.is_container
        if key_ti is not None:
            target_ti.register_contained_type(0, key_ti)
        if value_ti is not None:
            target_ti.register_contained_type(1, value_ti)
