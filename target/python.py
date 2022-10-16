from target.targetlanguage import AbstractTargetLanguage
from target.targetlanguage import CommonInfixFormatter
import asttoken


class PythonSyntax(AbstractTargetLanguage):

    def __init__(self):
        """
        : is the block start delim
        """
        super().__init__(formatter=PythonFormatter(),
                         is_prefix=False,
                         stmt_start_delim="", stmt_end_delim="",
                         block_start_delim=":", block_end_delim="",
                         flow_control_test_start_delim="", flow_control_test_end_delim="",
                         loop_foreach_keyword="in",
                         arg_delim=",",
                         strongly_typed=False,
                         explicit_rtn=True,
                         has_block_scope=False,
                         has_assignment_lhs_unpacking=True,
                         function_signature_template="def $func_name($args_start$arg_name, $args_end)")

        self.type_mapper.register_none_type_name("None")
        self.type_mapper.register_container_type_mapping(list, "list", "[", "]")
        self.type_mapper.register_container_type_mapping(tuple, "tuple", "(", ")")
        self.type_mapper.register_container_type_mapping(dict, "dict", "{", "}", ":")


class PythonFormatter(CommonInfixFormatter):

    def delim_suffix(self, token, remaining_tokens):
        if asttoken.is_boundary_starting_before_value_token(
                remaining_tokens, asttoken.BLOCK):
            # we want if <cond>: (no space between <cond> and :
            return False
        return super().delim_suffix(token, remaining_tokens)
