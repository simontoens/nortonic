import re
import collections

from visitor import visitor
import ast
import asttoken
import context
import templates
import types


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
        self.pass_by_value = True


class ContainerTypeMapping:

    def __init__(self, py_type, target_type_name,
                 start_literal, end_literal,
                 start_values_wrapper, end_values_wrapper,
                 value_separator, apply_if):
        self.py_type = py_type
        self.target_type_name = target_type_name
        self.start_literal = start_literal
        self.end_literal = end_literal
        self.start_values_wrapper = start_values_wrapper
        self.end_values_wrapper = end_values_wrapper
        self.value_separator = value_separator
        self.apply_if = apply_if
        self.is_container_type = True
        self.pass_by_value = False


class TypeCoercionRule:

    def __init__(self, result_type, rhs_conversion_function_name=None):
        self.result_type = result_type
        self.rhs_conversion_function_name = rhs_conversion_function_name



CONTAINED_TYPE_TOKEN = "$contained_type"


class TypeMapper:

    def __init__(self):
        self._py_type_to_type_mappings = collections.defaultdict(list)
        self._type_coercion_rule_mapping = {} # (lhs, rhs) -> rule

    def register_none_type_name(self, target_name):
        self.register_simple_type_mapping(type(None), target_name, lambda v: target_name)

    def register_simple_type_mapping(self, py_type, target_name, literal_converter=None):
        """
        """
        if isinstance(py_type, context.TypeInfo):
            # if the python type requires in import, it is easier to pass it in
            # as a TypeInfo constant
            py_type = py_type.value_type
        m = SimpleTypeMapping(py_type, target_name, literal_converter)
        self._py_type_to_type_mappings[py_type].append(m)

    def register_container_type_mapping(self, py_types, target_name,
                                        start_literal,
                                        end_literal,
                                        start_values_wrapper=None,
                                        end_values_wrapper=None,
                                        values_separator=None,
                                        apply_if=None):
        """
        apply_if - optional
            A function that takes a single argument, a TypeInfo instance.
            If this function is None, the mapping is applied.
            If the function is not None, the mapping is only applied if it
            returns True.
        """
        if not isinstance(py_types, (list, tuple)):
            py_types = [py_types]
        for py_type in py_types:
            m = ContainerTypeMapping(py_type, target_name,
                                     start_literal, end_literal,
                                     start_values_wrapper, end_values_wrapper,
                                     values_separator, apply_if)
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
        return target_type_name

    def replace_contained_type(self, type_info, target_type_name):
        # replace all $contained_type[<index>] tokens in the target_type_name
        type_parameter_index = None
        while CONTAINED_TYPE_TOKEN in target_type_name:
            pattern = "^.*?" + CONTAINED_TYPE_TOKEN.replace("$", "\\$") + "(\$\[(.*?)\]).*$"
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
        return sep.join(contained_type_names)

    def get_type_mapping(self, type_info):
        """
        Given a context.TypeInfo instance, returns the TypeMapping instance.
        """
        assert type_info is not None, "type_info cannot be None"
        py_type = type_info.value_type
        assert py_type is not None, "value_type cannot be None"
        type_mapping = self._get_type_mapping_for_py_type(py_type, type_info)
        assert type_mapping is not None, "cannot find a type mapping for %s" % type_info
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
        if isinstance(lhs_type, context.TypeInfo):
            lhs_type = lhs_type.value_type
        if isinstance(rhs_type, context.TypeInfo):
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
    def delim_suffix(self, token, remaining_tokens):
        if token.type.is_unaryop:
            # i = -1, not i = - 1
            return False
        return token.type.has_value

    def newline(self, token, remaining_tokens):
        return False


class CommonInfixFormatter(AbstractLanguageFormatter):

    def delim_suffix(self, token, remaining_tokens):
        if asttoken.next_token_has_type(remaining_tokens, asttoken.STMT, is_end=True):
            # we want foo; not foo ;
            return False
        if asttoken.next_token_has_type(remaining_tokens, asttoken.TARGET_DEREF):
            # no space before '.': "foo".startswith("f"), not "foo" .startswith
            return False
        if asttoken.is_boundary_starting_before_value_token(remaining_tokens, asttoken.BLOCK):
            # we want if (1 == 1) {, not if (1 == 1){
            return True
        if token.type.is_block and token.is_end:
            # we want } else, not }else
            return True
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
        if asttoken.next_token_has_type(remaining_tokens, asttoken.SEPARATOR):
            # {"key": "value"}, not {"key" : "value"}
            return False
        if token.type.is_sep and asttoken.is_within_boundary(remaining_tokens, asttoken.SUBSCRIPT):
            # "foo"[1:len(blah)], not "foo"[1: len(blah)]
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
        if token.type is asttoken.BINOP and asttoken.next_token_is(remaining_tokens, "="):
            # a += 1, not a + = 1
            return False
        return super().delim_suffix(token, remaining_tokens)

    def newline(self, token, remaining_tokens):
        if token.type.is_stmt and token.is_end:
            return True
        return super().newline(token, remaining_tokens)


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
                 equality_binop="==", identity_binop="is",
                 and_binop="&&", or_binop="||",
                 loop_foreach_keyword="for",
                 arg_delim=",",
                 strongly_typed=False,
                 explicit_rtn=False,
                 has_block_scope=False,
                 has_assignment_lhs_unpacking=False,
                 ternary_replaces_if_expr=False,
                 type_declaration_template=None,
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
        self.has_pointers = has_pointers

        self.visitors = [] # additional node visitors
        self.functions = {} # functions calls to rewrite
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
                module_name = py_type.get_metadata(context.TYPE_INFO_METADATA_MODULE_NAME)
                attr_path = "%s.%s" % (module_name, py_name)
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
