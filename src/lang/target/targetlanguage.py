from lang.target import rewrite as rewrite_targets
from lang.target import templates
import ast
import collections
import lang.internal.typeinfo as ti
import re
import types
import util.types
import visitor.asttoken as asttoken
import visitor.visitor as visitor


class Argument:
    """
    Describes a function (== method) argument.
    """

    def __init__(self, node, type):
        """
        node: the argument AST node
        type: the type of the argument
        """
        self.node = node
        self.type = type


class Importable:

    def __init__(self, imports):
        if imports is not None:
            if not isinstance(imports, (list, tuple)):
                imports = [imports]
        self.imports = imports


class RewriteRule(Importable):
    """
    Describes a function rewrite rule.
    """
    def __init__(self, py_name, py_type, target_name, function_rewrite=None,
                 imports=[]):
        super().__init__(imports)
        self.py_name = py_name
        self.py_type = py_type
        self.target_name = target_name
        self.function_rewrite = function_rewrite

    def __str__(self):
        return "[RewriteRule] %s" % self.py_name


class AbstractTypeMapping(Importable):

    def __init__(self, py_type, target_type_name, imports):
        super().__init__(imports)
        self.py_type = py_type
        self.target_type_name = target_type_name
        self.is_container_type = False


class SimpleTypeMapping(AbstractTypeMapping):

    def __init__(self, py_type, target_type_name, literal_converter):
        super().__init__(py_type, target_type_name, imports=[])
        self.literal_converter = literal_converter


class FunctionTypeMapping(AbstractTypeMapping):

    def __init__(self, function_type_template):
        super().__init__(types.FunctionType, "", imports=[])
        assert isinstance(function_type_template, (types.NoneType, templates.FunctionSignatureTemplate))
        self.function_type_template = function_type_template


class ContainerTypeMapping(AbstractTypeMapping):

    def __init__(self, py_type, target_type_name,
                 start_literal, end_literal,
                 start_values_wrapper, end_values_wrapper,
                 value_separator,
                 homogenous_types,
                 imports):
        super().__init__(py_type, target_type_name, imports)
        self.start_literal = start_literal
        self.end_literal = end_literal
        self.start_values_wrapper = start_values_wrapper
        self.end_values_wrapper = end_values_wrapper
        self.value_separator = value_separator
        self.is_container_type = True
        self.homogenous_types = homogenous_types
        if homogenous_types is None:
            """
            about 'apply_if':
                a function that takes a single argument, a TypeInfo instance.
                if this function is None, the mapping is applied.
                if the function is not None, the mapping is only applied if it
                returns True

                the apply_if filter could also be customized and passed in if
                a custom rule is needed ... right now the only use-case is
                based on homogeneous containers
            """
            self.apply_if = None
        else:
            assert isinstance(homogenous_types, bool)
            if homogenous_types:
                self.apply_if = lambda type_info: type_info.contains_homogeneous_types
            else:
                self.apply_if = lambda type_info: not type_info.contains_homogeneous_types


class TypeCoercionRule:

    def __init__(self, result_type, rhs_conversion_function_name=None):
        self.result_type = result_type
        self.rhs_conversion_function_name = rhs_conversion_function_name

    def __str__(self):
        return "type coercion rule '%s' -> %s" % (self.rhs_conversion_function_name, self.result_type)


CONTAINED_TYPE_TOKEN = "$contained_type"


class TypeMapper:

    def __init__(self, dynamically_typed):
        self._dynamically_typed = dynamically_typed
        self._py_type_to_type_mappings = collections.defaultdict(list)
        self._type_coercion_rule_mapping = {} # (lhs, rhs) -> rule

    def register_none_type_name(self, target_name):
        self.register_simple_type_mapping(type(None), target_name, lambda v: target_name)

    def register_simple_type_mapping(self, py_type, target_name, literal_converter=None):
        assert py_type is not types.FunctionType, "functions are  not a simple!"
        if isinstance(py_type, ti.TypeInfo):
            # if the python type requires in import, it is easier to pass it in
            # as a TypeInfo constant
            py_type = py_type.value_type
        m = SimpleTypeMapping(py_type, target_name, literal_converter)
        self._py_type_to_type_mappings[py_type].append(m)

    def register_function_type_mapping(self, template):
        m = FunctionTypeMapping(template)
        self._py_type_to_type_mappings[m.py_type].append(m)

    def register_container_type_mapping(self, py_types, target_name,
                                        start_literal,
                                        end_literal,
                                        start_values_wrapper=None,
                                        end_values_wrapper=None,
                                        values_separator=None,
                                        homogenous_types=None,
                                        imports=[]):
        """
        homogenous_types:
            if None, this one is ignored.
            if True, this mapping is only used if the container has homogenous
            types.
            if Frue, this mapping is only used if the container does not have
            homogenous types.
        """
        if not isinstance(py_types, (list, tuple)):
            py_types = [py_types]
        for py_type in py_types:
            m = ContainerTypeMapping(py_type, target_name,
                                     start_literal, end_literal,
                                     start_values_wrapper, end_values_wrapper,
                                     values_separator,
                                     homogenous_types,
                                     imports)
            self._py_type_to_type_mappings[py_type].append(m)

    def lookup_target_type_name(self, type_info):
        """
        Given a context.TypeInfo instance, returns the type name as a string.
        """
        type_mapping = self.get_type_mapping(type_info)
        target_type_name = type_mapping.target_type_name
        if type_info.is_pointer:
            target_type_name = "*%s" % target_type_name
        if type_mapping.is_container_type:
            if CONTAINED_TYPE_TOKEN in target_type_name:
                target_type_name = self.replace_contained_type(type_info, target_type_name)
        if type_info.is_function and type_mapping.function_type_template is not None:
            # leaky abstraction below - we need a diff template?
            # we are generating the func type, therefore there is no func name
            # similarly, the argumment names are unknown / do not make sense
            func_name = ""
            func = type_info.function
            arguments = [("", self.lookup_target_type_name(ti)) for ti in func.arg_type_infos]
            rtn_type = self.lookup_target_type_name(func.get_rtn_type_info())
            target_type_name = type_mapping.function_type_template.render(
                func_name, arguments, rtn_type, visibility="", scope="", node_attrs=[])
        return target_type_name

    def replace_contained_type(self, type_info, target_type_name):
        # replace all $contained_type[<index>] tokens in the target_type_name
        type_parameter_index = None
        while CONTAINED_TYPE_TOKEN in target_type_name:
            pattern = "^.*?" + CONTAINED_TYPE_TOKEN.replace("$", "\\$") + "(\\$\\[(.*?)\\]).*$"
            m = re.search(pattern, target_type_name)
            if m is not None:
                if m.group(2) == "":
                    # $contained_type$[] means all contained types
                    type_parameter_index = None
                else:
                    type_parameter_index = int(m.group(2))
                # remove the $[<num>] expr
                target_type_name = target_type_name[0:m.start(1)] + target_type_name[m.end(1):]
            contained_target_type_names = self.lookup_contained_type_names(
                type_info, sep=", ", type_parameter_index=type_parameter_index)
            target_type_name = target_type_name.replace(
                CONTAINED_TYPE_TOKEN, contained_target_type_names, 1)
        return target_type_name

    def lookup_contained_type_names(self, type_info, sep, type_parameter_index=None):
        contained_type_names = []
        for i, cti in enumerate(type_info.get_contained_type_infos()):
            ttn = self.lookup_target_type_name(cti)
            if type_parameter_index is None or type_parameter_index == i:
                contained_type_names.append(ttn)
        assert len(contained_type_names) > 0, "contained type name(s) should not be empty for %s" % type_info
        return sep.join(contained_type_names)

    def get_type_mapping(self, type_info):
        """
        Given a context.TypeInfo instance, returns the TypeMapping instance.
        """
        assert type_info is not None, "type_info cannot be None"
        py_type = type_info.value_type
        assert py_type is not None, "value_type cannot be None"
        type_mapping = self._get_type_mapping_for_py_type(py_type, type_info)
        if type_mapping is None:
            if self._dynamically_typed:
                # for dynamically typed languages we make up dummy types
                if type_info.is_function:
                    return FunctionTypeMapping(function_type_template=None)
                else:
                    # __name__ to get "str" instead of "<class 'str'>"
                    return SimpleTypeMapping(py_type, py_type.__name__, literal_converter=None)
            else:
                raise Exception("No type mapping registered for [%s]" % py_type)
        return type_mapping

    def convert_to_literal(self, value):
        value_type = value if isinstance(value, type) else type(value)
        type_mapping = self._get_type_mapping_for_py_type(value_type)
        if type_mapping is not None:
            assert not type_mapping.is_container_type
            if type_mapping.literal_converter is not None:
                return type_mapping.literal_converter(value)
        return None

    def register_type_coercion_rule(self, lhs_type, rhs_type, result_type,
                                    rhs_conversion_function_name=None):
        r = TypeCoercionRule(result_type, rhs_conversion_function_name)
        self._type_coercion_rule_mapping[(lhs_type, rhs_type)] = r

    def lookup_type_coercion_rule(self, lhs_type, rhs_type):
        if isinstance(lhs_type, ti.TypeInfo):
            lhs_type = lhs_type.value_type
        if isinstance(rhs_type, ti.TypeInfo):
            rhs_type = rhs_type.value_type
        return self._type_coercion_rule_mapping.get((lhs_type, rhs_type), None)

    def _get_type_mapping_for_py_type(self, py_type, type_info=None):
        mappings = self._py_type_to_type_mappings[py_type]
        if len(mappings) == 0:
            return None
        if len(mappings) == 1:
            return mappings[0]
        else:
            if type_info is not None:
                mappings_with_condition = [m for m in mappings if m.apply_if is not None]            
                for m in mappings_with_condition:
                    if m.apply_if(type_info):
                        return m
            mappings_without_condition = [m for m in mappings if m.apply_if is None]                
            if len(mappings_without_condition) == 0:
                return None
            return mappings_without_condition[0]


class AbstractLanguageFormatter:
    """
    Formatting customizations.
    """
    def __init__(self, blocks_close_on_same_line=False):
        self.blocks_close_on_same_line = blocks_close_on_same_line

    def requires_space_sep(self, token, remaining_tokens):
        if token.type.is_unaryop:
            # i = -1, not i = - 1
            return False
        return token.type.has_value


class CommonInfixFormatter(AbstractLanguageFormatter):

    def requires_space_sep(self, token, remaining_tokens):
        """
        The rules defined in this method live here instead of in asttoken
        because they have to be replaceable by different target language
        implementation.
        """
        if asttoken.is_boundary_starting_before_value_token(remaining_tokens, asttoken.BLOCK):
            # we want if (1 == 1) {, not if (1 == 1){
            return True
        if token.type.is_func_call_boundary and token.is_end and asttoken.next_token_has_value(remaining_tokens):
            # "foo".length() == 3, not "foo".length()== 3;
            return True
        if asttoken.next_token_has_type(remaining_tokens, asttoken.FUNC_CALL_BOUNDARY) and remaining_tokens[0].is_start:
            # "foo".endswith("blah"), not "foo".endswith ("blah")
            return False
        if token.type.is_container_literal_boundary:
            # no space before first container literal arg, for example for list:
            # ["a", ... instead of [ "a", ...
            return False
        if asttoken.is_boundary_ending_before_value_token(remaining_tokens, asttoken.FUNC_ARG):
            # no space after func arg: 1, 2 - not 1 , 2
            return False
        return super().requires_space_sep(token, remaining_tokens)


class AbstractTargetLanguage:
    """
    Describes a target language.
    """
    def __init__(self, formatter,
                 is_prefix=False,
                 stmt_end_delim=";", stmt_end_delim_always_required=False,
                 block_start_delim="", block_end_delim="",
                 flow_control_test_start_delim="",
                 flow_control_test_end_delim="",
                 not_unaryop="!", # [!]true, [not] True ...
                 # equality
                 eq_binop="==", not_eq_binop="!=",
                 # identity
                 same_binop="==", not_same_binop="!=",
                 and_binop="&&", or_binop="||",
                 loop_foreach_keyword="in",
                 arg_delim=",",
                 # whether all types must be mapped, if False every Python type
                 # must have an explicit mapping - this is only required if
                 # types actually appear in generated code
                 # also, if False, enables additional type assertions
                 dynamically_typed=False,
                 explicit_rtn=True,
                 has_block_scope=False,
                 has_assignment_lhs_unpacking=False,
                 # whether the target language has Python-style if expressions:
                 # for example: a = 2 if 0 > 1 else 1
                 has_if_expr=False,
                 # rewrites Python's if expression as a ternary if expression:
                 #   a = 2 if 0 > 1 else 1 ->
                 #   a = 0 > 1 ? 2 : 1
                 ternary_replaces_if_expr=False,
                 type_declaration_template=None,
                 anon_function_signature_template=None,
                 function_signature_template=None,
                 function_can_return_multiple_values=False,
                 has_pointers=False):
        self.formatter = formatter
        self.is_prefix = is_prefix
        self.stmt_end_delim = stmt_end_delim
        self.stmt_end_delim_always_required = stmt_end_delim_always_required
        self.block_start_delim = block_start_delim
        self.block_end_delim = block_end_delim
        self.flow_control_test_start_delim = flow_control_test_start_delim
        self.flow_control_test_end_delim = flow_control_test_end_delim
        self.eq_binop = eq_binop
        self.not_eq_binop = not_eq_binop
        self.same_binop = same_binop
        self.not_same_binop = not_same_binop
        self.and_binop = and_binop
        self.or_binop = or_binop
        self.not_unaryop = not_unaryop
        self.loop_foreach_keyword = loop_foreach_keyword
        self.arg_delim = arg_delim
        self.dynamically_typed = dynamically_typed
        self.explicit_rtn = explicit_rtn
        self.has_block_scope = has_block_scope
        self.has_assignment_lhs_unpacking = has_assignment_lhs_unpacking
        self.has_if_expr = has_if_expr
        self.ternary_replaces_if_expr = ternary_replaces_if_expr
        if isinstance(type_declaration_template, str):
            type_declaration_template = templates.TypeDeclarationTemplate(type_declaration_template)
        self.type_declaration_template = type_declaration_template
        if isinstance(anon_function_signature_template, str):
            anon_function_signature_template = templates.FunctionSignatureTemplate(anon_function_signature_template)
        self.anon_function_signature_template = anon_function_signature_template
        if isinstance(function_signature_template, str):
            function_signature_template = templates.FunctionSignatureTemplate(function_signature_template)
        self.function_signature_template = function_signature_template
        self.function_can_return_multiple_values = function_can_return_multiple_values
        self.has_pointers = has_pointers

        self.visitors = [] # additional node visitors
        self.functions = {} # functions calls to rewrite
        self.type_mapper = TypeMapper(dynamically_typed)

    def to_literal(self, value):
        value_type = value if isinstance(value, type) else type(value)
        if value_type is str:
            return '"%s"' % str(value)
        literal = self.type_mapper.convert_to_literal(value)
        return value if literal is None else literal

    def to_identifier(self, value):
        return str(value)

    def combine_types(self, lhs, rhs):
        # default type coercion
        if lhs is str:
            return str
        if lhs is float or rhs is float:
            return float
        return int

    def register_node_visitor(self, visitor):
        self.visitors.append(visitor)

    def get_function_lookup_key(self, func_name, target_type, ast_path, target_node_type):
        if target_node_type is not ast.Attribute:
            target_node_type = None
        return "name:%s target_type:%s path:%s node_type:%s" % (func_name,
                                  str(target_type),
                                  str(ast_path),
                                  str(target_node_type))

    def register_function_rewrite(self, py_name, py_type, rewrite,
                                  target_name=None, imports=[]):
        """
        Registers a function rewrite.

        target_name may be set if the function has to be only renamed (but
        perhaps the function arguments have to be re-written),
        """
        self._register_function_rewrite(py_name, py_type, rewrite,
                                        target_name, ast.Call, imports)

    def register_attribute_rewrite(self, py_name, py_type, rewrite,
                                   target_name=None, imports=[]):
        """
        Registers an attribute rewrite, for example os.path.sep.
        """
        self._register_function_rewrite(py_name, py_type, rewrite, target_name,
                                        ast.Attribute, imports)

    def register_rename(self, symbol, to, arg_type=None, inst_type=None,
                        imports=[]):
        self.register_rewrite(symbol, rewrite=None, rename_to=to,
                              arg_type=arg_type, inst_type=inst_type,
                              imports=imports)

    def register_rewrite(self, symbol, rewrite, arg_type=None, inst_type=None,
                         rename_to=None, imports=[]):
        if symbol is rewrite_targets.ALL:
            # special case - rewrite all rules at once!
            self.functions[rewrite_targets.ALL] = RewriteRule(
                rewrite_targets.ALL, None, None, function_rewrite=rewrite)
        else:
            assert isinstance(symbol, (str, rewrite_targets.RewriteTarget)) or util.types.instanceof_py_function(symbol), "Unexpected %s" % symbol
            name = symbol
            py_type = None
            if isinstance(symbol, rewrite_targets.RewriteTarget):
                name = name.name
            elif util.types.instanceof_py_function(symbol):
                name = util.types.get_py_function_name(symbol)
                assert name is not None
                py_type = util.types.get_receiver_type(symbol)
            if py_type is None:
                # weird restriction ... ?
                assert arg_type is None or inst_type is None
                py_type = arg_type if arg_type is not None else inst_type
            self._register_rewrite(name, rewrite, py_type, rename_to, imports)

    def _register_rewrite(self, name, rewrite, py_types=None, rename_to=None,
                          imports=[]):
        """
        name - the indentifier or keyword to rewrite, for example "len" or "if"
        rewrite - a function that takes args and an ASTRewriter instance
        py_types - either the argument type(s) or the instance type(s) of "name"
        rename_to - for simple renames, the new name
        imports - iterable of required imports
        """
        if isinstance(py_types, (type, types.NoneType)):
            # multiple types may be specified as a convenience
            py_types = [py_types,]
        for py_type in py_types:
            self._register_function_rewrite(name, py_type, rewrite,
                                            rename_to, ast.Call, imports)

    def _register_function_rewrite(self, py_name, py_type, rewrite, target_name, target_node_type, imports=[], rewritten_symbol=None):
        assert py_name is not None
        attr_path = None
        if isinstance(py_type, ti.TypeInfo):
            # py_type may be passed in as native type or wrapped
            if py_type.value_type is types.ModuleType:
                attr_path = "%s.%s" % (py_type.module_name, py_name)
            py_type = py_type.value_type
        key = self.get_function_lookup_key(py_name, py_type, attr_path, target_node_type)
        assert not key in self.functions, "duplicate rewrite %s" % key
        function = RewriteRule(py_name, py_type, target_name=target_name,
                               function_rewrite=rewrite, imports=imports)
        self.functions[key] = function


class NodeVisitor(visitor.NoopNodeVisitor):
    """
    Target languages may provide visitor instances.
    """

    def __init__(self):
        super().__init__(delegate=None)
        self.context = None # will be set before the visitor is put into action
