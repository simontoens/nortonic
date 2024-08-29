import lang.target.rewrite as rewrite
import lang.target.targetlanguage as targetlanguage
import visitor.asttoken as asttoken
import visitor.nodeattrs as nodeattrs


class PythonSyntax(targetlanguage.AbstractTargetLanguage):

    def __init__(self):
        super().__init__(formatter=PythonFormatter(),
                         is_prefix=False,
                         stmt_end_delim=";", stmt_end_delim_always_required=False,
                         block_start_delim=":", block_end_delim="",
                         flow_control_test_start_delim="", flow_control_test_end_delim="",
                         same_binop="is", not_same_binop="is not",
                         and_binop="and", or_binop="or",
                         not_unaryop="not",
                         loop_foreach_keyword="in",
                         arg_delim=",",
                         dynamically_typed=True,
                         has_if_expr=True,
                         has_block_scope=False,
                         has_assignment_lhs_unpacking=True,
                         type_declaration_template="$identifier = $rhs",
                         anon_function_signature_template="lambda $args_start$arg_name,$args_end",
                         function_signature_template="def $func_name($args_start$arg_name, $args_end)",
                         #function_signature_template="def $func_name($args_start$arg_name: $arg_type, $args_end) -> $rtn_type",
                         function_can_return_multiple_values=False)

        self.type_mapper.register_none_type_name("None")
        self.type_mapper.register_container_type_mapping(list, "list", "[", "]")
        self.type_mapper.register_container_type_mapping(tuple, "tuple", "(", ")")
        self.type_mapper.register_container_type_mapping(dict, "dict",
                                                         "{", "}",
                                                         values_separator=":")

        # a rewrite hook for all symbols so that we can register imports
        def _register_imports(args, rw):
            f = nodeattrs.get_function(rw.node, must_exist=False)
            if f is not None:
                rw.register_imports(f.imports)
        self.register_rewrite(rewrite.ALL, rewrite=_register_imports)


class PythonFormatter(targetlanguage.CommonInfixFormatter):

    def __init__(self):    
        super().__init__(blocks_close_on_same_line=False)

    def requires_space_sep(self, token, remaining_tokens):
        if asttoken.is_boundary_starting_before_value_token(
                remaining_tokens, asttoken.BLOCK):
            # we want if <cond>: (no space between <cond> and :)
            return False
        if token.type.is_unaryop and token.value == "not":
            # not True instead of notTrue
            return True
        return super().requires_space_sep(token, remaining_tokens)
