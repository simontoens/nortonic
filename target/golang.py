from target.targetlanguage import AbstractTargetLanguage
from target.targetlanguage import CommonInfixFormatter
import asttoken


class GolangSyntax(AbstractTargetLanguage):

    def __init__(self):
        super().__init__(formatter=CommonInfixFormatter(),
                         is_prefix=False,
                         stmt_start_delim="", stmt_end_delim="",
                         block_start_delim="{", block_end_delim="}",
                         flow_control_test_start_delim="", flow_control_test_end_delim="",
                         loop_foreach_keyword="in",
                         arg_delim=",",
                         strongly_typed=False,
                         explicit_rtn=True,
                         has_block_scope=False,
                         has_assignment_lhs_unpacking=True,
                         type_declaration_template="$identifier := ",
                         function_signature_template="def $func_name($args_start$arg_name, $args_end)")

        self.type_mapper.register_none_type_name("nil")
        self.type_mapper.register_type_coercion_rule(str, int, str, "string")
        self.type_mapper.register_type_coercion_rule(str, float, str, "string")

        self.register_function_rename(py_name="print", py_type=None,
                                      target_name="fmt.Println")
        

class AssignmentVisitor:
    pass
