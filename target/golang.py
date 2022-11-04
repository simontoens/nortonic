from target.targetlanguage import AbstractTargetLanguage
from target.targetlanguage import CommonInfixFormatter
from target.targetlanguage import NodeVisitor
import templates


class Options:
    pass

NO_TYPE_INF = Options()


class GolangTypeDeclarationTemplate(templates.TypeDeclarationTemplate):

    def __init__(self):
        super().__init__("$identifier := ")

    def pre_render__hook(self, declaration, owning_scope, node_metadata):
        return "var $identifier $type = " if NO_TYPE_INF in node_metadata else declaration


class AssignmentVisitor(NodeVisitor):

    def assign(self, node, num_children_visited):
        if num_children_visited == -1:
            #node.get_metadata()[NO_TYPE_INF] = True
            pass


class GolangSyntax(AbstractTargetLanguage):

    def __init__(self):
        super().__init__(formatter=CommonInfixFormatter(),
                         is_prefix=False,
                         stmt_start_delim="", stmt_end_delim="",
                         block_start_delim="{", block_end_delim="}",
                         flow_control_test_start_delim="", flow_control_test_end_delim="",
                         loop_foreach_keyword="in",
                         arg_delim=",",
                         strongly_typed=True,
                         explicit_rtn=True,
                         has_block_scope=False,
                         has_assignment_lhs_unpacking=True,
                         type_declaration_template=GolangTypeDeclarationTemplate(),
                         function_signature_template="def $func_name($args_start$arg_name, $args_end)")

        self.register_node_visitor(AssignmentVisitor())

        self.type_mapper.register_none_type_name("nil")
        self.type_mapper.register_simple_type_mapping(int,  "int")
        self.type_mapper.register_simple_type_mapping(str,  "string")
        self.type_mapper.register_simple_type_mapping(float,  "float32")

        self.type_mapper.register_type_coercion_rule(str, int, str, "string")
        self.type_mapper.register_type_coercion_rule(str, float, str, "string")

        self.register_function_rename(py_name="print", py_type=None,
                                      target_name="fmt.Println")
