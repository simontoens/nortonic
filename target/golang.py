from target.targetlanguage import AbstractTargetLanguage
from target.targetlanguage import CommonInfixFormatter
from target.targetlanguage import NodeVisitor
import asttoken
import templates


class Options:
    pass

NO_TYPE_INF = Options()


class GolangTypeDeclarationTemplate(templates.TypeDeclarationTemplate):

    def __init__(self):
        super().__init__("$identifier := ")

    def pre_render__hook(self, declaration, scope, node_metadata):
        return "var $identifier $type = " if NO_TYPE_INF in node_metadata else declaration


class AssignmentVisitor(NodeVisitor):

    def assign(self, node, num_children_visited):
        if num_children_visited == -1:
            #node.get_metadata()[NO_TYPE_INF] = True
            pass


class GolangSyntax(AbstractTargetLanguage):

    def __init__(self):
        super().__init__(formatter=GolangFormatter(),
                         is_prefix=False,
                         stmt_start_delim="", stmt_end_delim="",
                         block_start_delim="{", block_end_delim="}",
                         flow_control_test_start_delim="",
                         flow_control_test_end_delim="",
                         equality_binop = "==", identity_binop="==",
                         and_binop="&&", or_binop="||",
                         loop_foreach_keyword="in",
                         arg_delim=",",
                         strongly_typed=True,
                         explicit_rtn=True,
                         has_block_scope=False,
                         has_assignment_lhs_unpacking=False,
                         type_declaration_template=GolangTypeDeclarationTemplate(),
                         function_signature_template="func $func_name($args_start$arg_name $arg_type, $args_end) $rtn_type",
                         function_can_return_multiple_values=True)

        self.register_node_visitor(AssignmentVisitor())

        self.type_mapper.register_none_type_name("nil")
        self.type_mapper.register_simple_type_mapping(bool, "bool")
        self.type_mapper.register_simple_type_mapping(int, "int")
        self.type_mapper.register_simple_type_mapping(str, "string")
        self.type_mapper.register_simple_type_mapping(float, "float32")

        self.type_mapper.register_container_type_mapping(
            list,
            "[]$contained_type${1}",
            start_literal="[]$contained_type${1}{",
            end_literal="}")

        # TODO - review best way to represent a tuple in Golang
        self.type_mapper.register_container_type_mapping(
            tuple,
            "[]Tuple",
            start_literal="[]$contained_type${1}{",
            end_literal="}")

        # non-homogeneous tuple
        self.type_mapper.register_container_type_mapping(
            tuple,
            "[]$contained_type${*}",
            start_literal="[]$contained_type${*}{",
            end_literal="}",
            apply_if=lambda type_info: not type_info.contains_homogeneous_types)

        self.type_mapper.register_type_coercion_rule(str, int, str, "string")
        self.type_mapper.register_type_coercion_rule(str, float, str, "string")

        self.register_function_rename(py_name="print", py_type=None,
                                      target_name="fmt.Println")

        self.register_function_rewrite(
            py_name="input", py_type=str,
            target_name="bufio.NewReader(os.Stdin).ReadString",
            rewrite=lambda args, rw:
                rw.insert_above(rw.call("fmt.Print").append_arg(args[0]))
                  .replace_args_with('\\n'))

        self.register_function_rewrite(
            py_name="append", py_type=list,
            rewrite=lambda args, rw: rw
                .rewrite_as_func_call(inst_1st=True)
                .reassign_to_arg())

    def to_literal(self, value):
        v = super().to_literal(value)
        if isinstance(v, str):
            # generalize please - needs to be set on node being rewritten?
            if v == '"\\n"':
                # single char, use single quotes
                return "'%s'" % v[1:-1]
        return v

class GolangFormatter(CommonInfixFormatter):

    def delim_suffix(self, token, remaining_tokens):
        if asttoken.is_boundary_starting_before_value_token(
                remaining_tokens, asttoken.BLOCK):
            # we want func foo() {, not func foo(){
            return True
        if token.type.is_block and token.is_end:
            # we want } else, not }else
            return True
        return super().delim_suffix(token, remaining_tokens)
