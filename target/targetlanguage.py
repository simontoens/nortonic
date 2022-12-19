import ast
import asttoken
import context
import templates
import types
import visitor


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


class Function:
    """
    Describes a function invocation.
    """
    def __init__(self, py_name, py_type, target_name, function_rewrite=None):
        self.py_name = py_name
        self.py_type = py_type
        self.target_name = target_name
        self.function_rewrite = function_rewrite

    def __str__(self):
        return "[Function] %s" % self.py_name


class SimpleTypeMapping:

    def __init__(self, py_type, target_type_name, literal_converter):
        self.py_type = py_type
        self.target_type_name = target_type_name
        self.literal_converter = literal_converter
        self.is_container_type = False


class ContainerTypeMapping:

    def __init__(self, py_type, target_type_name,
                 start_literal, end_literal,
                 start_values_wrapper, end_values_wrapper,
                 value_separator):
        self.py_type = py_type
        self.target_type_name = target_type_name
        self.start_literal = start_literal
        self.end_literal = end_literal
        self.start_values_wrapper = start_values_wrapper
        self.end_values_wrapper = end_values_wrapper
        self.value_separator = value_separator
        self.is_container_type = True


class TypeCoercionRule:

    def __init__(self, result_type, rhs_conversion_function_name=None):
        self.result_type = result_type
        self.rhs_conversion_function_name = rhs_conversion_function_name


class TypeMapper:

    def __init__(self):
        self._py_type_to_type_mapping = {}
        self._type_coercion_rule_mapping = {} # (lhs, rhs) -> rule

    def register_none_type_name(self, target_name):
        self.register_simple_type_mapping(type(None), target_name, lambda v: target_name)

    def register_simple_type_mapping(self, py_type, target_name, literal_converter=None):
        if isinstance(py_type, context.TypeInfo):
            # if the python type requires in import, it is easier to pass it in
            # as a TypeInfo constant
            py_type = py_type.value_type
        m = SimpleTypeMapping(py_type, target_name, literal_converter)
        self._py_type_to_type_mapping[py_type] = m

    def register_container_type_mapping(self, py_type, target_name,
                                        start_literal,
                                        end_literal,
                                        start_values_wrapper=None,
                                        end_values_wrapper=None,
                                        values_separator=None):
        m = ContainerTypeMapping(py_type, target_name,
                                 start_literal, end_literal,
                                 start_values_wrapper, end_values_wrapper,
                                 values_separator)
        self._py_type_to_type_mapping[py_type] = m

    def lookup_target_type_name(self, type_info):
        """
        Given a context.TypeInfo instance, returns the type name of the target
        syntax, as a string.
        """
        assert type_info is not None, "TypeInfo instance cannot be None"
        py_type = self._get_py_type(type_info)
        assert py_type is not None, "lookup_target_type_name: py_type cannot be None"
        if py_type is None.__class__:
            return None
        assert py_type in self._py_type_to_type_mapping, "Missing type mapping for %s" % py_type
        type_mapping = self._py_type_to_type_mapping[py_type]
        target_type_name = type_mapping.target_type_name
        if type_mapping.is_container_type:
            if "$contained_type" in target_type_name:
                # replace $contained_type with the container's contained type
                contained_target_type_names = self.lookup_contained_type_names(type_info)
                target_type_name = target_type_name.replace("$contained_type", contained_target_type_names)
        return target_type_name

    def lookup_contained_type_names(self, type_info):
        contained_type_names = []
        for cti in type_info.get_contained_type_infos():
            ttn = self.lookup_target_type_name(cti)
            contained_type_names.append(ttn)
        return ", ".join(contained_type_names)

    def get_type_mapping(self, type_info):
        """
        Given a context.TypeInfo instance, returns the TypeMapping instance.
        syntax, as a string.
        """
        py_type = self._get_py_type(type_info)
        return self._py_type_to_type_mapping[py_type]

    def convert_to_literal(self, value):
        value_type = value if isinstance(value, type) else type(value)
        type_mapping = self._py_type_to_type_mapping.get(value_type, None)
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
        if isinstance(lhs_type, context.TypeInfo):
            lhs_type = lhs_type.value_type
        if isinstance(rhs_type, context.TypeInfo):
            rhs_type = rhs_type.value_type
        return self._type_coercion_rule_mapping.get((lhs_type, rhs_type), None)

    def _get_py_type(self, type_info):
        return type_info.value_type


class AbstractLanguageFormatter:
    """
    Formatting customizations.
    """
    def delim_suffix(self, token, remaining_tokens):
        if token.type.is_unaryop:
            # i = -1, not i = - 1
            return False
        return token.type.has_value

    def newline(self, token, remaining_tokens):
        return False


class CommonInfixFormatter(AbstractLanguageFormatter):

    def delim_suffix(self, token, remaining_tokens):
        if asttoken.next_token_has_type(remaining_tokens, asttoken.TARGET_DEREF):
            # no space before '.': "foo".startswith("f"), not "foo" .startswith
            return False
        if token.type.is_target_deref:
            # no space after '.': "foo".startswith("f")
            return False
        if token.type.is_func_call_boundary and token.is_end and asttoken.next_token_has_value(remaining_tokens):
            # "foo".length() == 3, not "foo".length()== 3;
            return True
        if asttoken.next_token_has_type(remaining_tokens, asttoken.FUNC_CALL_BOUNDARY) and remaining_tokens[0].is_start:
            # "foo".endswith("blah"), not "foo".endswith ("blah")
            return False
        if asttoken.is_boundary_ending_before_value_token(remaining_tokens, asttoken.FUNC_CALL_BOUNDARY):
            # no space after last func arg: ...,"foo")
            return False
        if asttoken.next_token_has_type(remaining_tokens, asttoken.SUBSCRIPT):
            # no space before subscript start: l[0] not l [0]
            # no space before subscript end: l[0] not l[0 ]
            return False
        if token.type.is_subscript and token.is_end:
            # d["k2"] = 3, not d["k2"]= 3
            return True
        if token.type.is_container_literal_boundary:
            # no space before first container literal arg, for example for list:
            # ["a", ... instead of [ "a", ...
            return False
        if asttoken.is_boundary_ending_before_value_token(remaining_tokens, asttoken.CONTAINER_LITERAL_BOUNDARY):
            # no space after last container literal arg, for example for list:
            # [..., "foo"] instead of [..., "foo" ]
            return False
        if asttoken.next_token_has_type(remaining_tokens, asttoken.VALUE_SEPARATOR):
            # {"key": "value"}, not {"key" : "value"}
            return False
        if token.type.is_value_sep and asttoken.next_next_token_has_type(remaining_tokens, asttoken.SUBSCRIPT, is_end=True):
            # "foo"[1:2], not "foo"[1: 2]
            return False
        if token.type.is_value_sep and asttoken.next_token_has_type(remaining_tokens, asttoken.UNARYOP) and asttoken.next_next_token_has_type(remaining_tokens[1:], asttoken.SUBSCRIPT, is_end=True):
            # "foo"[1:-2], not "foo"[1: -2]
            return False
        if asttoken.is_boundary_ending_before_value_token(remaining_tokens, asttoken.FUNC_ARG):
            # no space after func arg: 1, 2 - not 1 , 2
            return False
        if token.type.is_func_arg and token.is_end:
            # space after arg sep: 1, 2 - not 1,2
            return True
        if (token.type.is_binop_prec and token.is_end and asttoken.next_token_has_value(remaining_tokens)):
            # (1 + 1) * 2, not (1 + 1)* 2
            return True
        if asttoken.is_boundary_ending_before_value_token(remaining_tokens, asttoken.BINOP_PREC_BIND):
            # (2 + 3 * 4), not (2 + 3 * 4 )
            return False
        return super().delim_suffix(token, remaining_tokens)

    def newline(self, token, remaining_tokens):
        if token.type.is_stmt and token.is_end:
            return True
        return super().newline(token, remaining_tokens)


class AbstractTargetLanguage:
    """
    Stateless metadata that describes a target language.
    """

    def __init__(self, formatter,
                 is_prefix=None,
                 stmt_start_delim=None, stmt_end_delim=None,
                 block_start_delim=None, block_end_delim=None,
                 flow_control_test_start_delim=None,
                 flow_control_test_end_delim=None,
                 equality_binop=None, identity_binop=None,
                 and_binop=None, or_binop=None,
                 loop_foreach_keyword=None,
                 arg_delim=None,
                 strongly_typed=None,
                 explicit_rtn=None,
                 has_block_scope=None,
                 has_assignment_lhs_unpacking=None,
                 ternary_replaces_if_expr=None,
                 type_declaration_template=None,
                 function_signature_template=None,
                 function_can_return_multiple_values=None):
        self.formatter = formatter
        self.is_prefix = is_prefix
        self.stmt_start_delim = stmt_start_delim
        self.stmt_end_delim = stmt_end_delim
        self.block_start_delim = block_start_delim
        self.block_end_delim = block_end_delim
        self.flow_control_test_start_delim = flow_control_test_start_delim
        self.flow_control_test_end_delim = flow_control_test_end_delim
        self.equality_binop = equality_binop
        self.identity_binop = identity_binop
        self.and_binop = and_binop
        self.or_binop = or_binop
        self.loop_foreach_keyword = loop_foreach_keyword
        self.arg_delim = arg_delim
        self.strongly_typed = strongly_typed
        self.explicit_rtn = explicit_rtn
        self.has_block_scope = has_block_scope
        self.has_assignment_lhs_unpacking = has_assignment_lhs_unpacking
        self.ternary_replaces_if_expr = ternary_replaces_if_expr
        if isinstance(type_declaration_template, str):
            type_declaration_template = templates.TypeDeclarationTemplate(type_declaration_template)
        self.type_declaration_template = type_declaration_template
        if isinstance(function_signature_template, str):
            function_signature_template = templates.FunctionSignatureTemplate(function_signature_template)
        self.function_signature_template = function_signature_template
        self.function_can_return_multiple_values = function_can_return_multiple_values

        self.visitors = [] # additional node visitors
        self.functions = {} # functions_calls_to_rewrite
        self.type_mapper = TypeMapper()

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

    def register_function_rename(self, py_name, py_type, target_name):
        """
        Registers a function rename, for example endswith -> endsWith.
        """
        self.register_function_rewrite(py_name, py_type, rewrite=None, target_name=target_name)

    def register_function_rewrite(self, py_name, py_type, rewrite, target_name=None):
        """
        Registers a function rewrite.

        target_name may be set if the function has to be only renamed (but
        perhaps the function arguments have to be re-written),
        """
        self._register_function_rewrite(py_name, py_type, rewrite, target_name, ast.Call)

    def register_attribute_rewrite(self, py_name, py_type, rewrite, target_name=None):
        """
        Registers an attribute rewrite, for example os.path.sep.
        """
        self._register_function_rewrite(py_name, py_type, rewrite, target_name, ast.Attribute)

    def _register_function_rewrite(self, py_name, py_type, rewrite, target_name, target_node_type):
        attr_path = None
        if isinstance(py_type, context.TypeInfo):
            # py_type may be passed in as "native type" or wrapped
            if py_type.value_type is types.ModuleType:
                attr_path = "%s.%s" % (py_type.metadata, py_name)
            py_type = py_type.value_type
        key = self.get_function_lookup_key(py_name, py_type, attr_path, target_node_type)
        assert not key in self.functions, "duplicate rewrite %s" % key
        function = Function(py_name, py_type, target_name=target_name, function_rewrite=rewrite)
        self.functions[key] = function


class NodeVisitor(visitor.NoopNodeVisitor):
    """
    Target languages may provide visitor instances.
    """

    def __init__(self):
        super().__init__(delegate=None)
        self.ast_context = None
