from target.targetlanguage import AbstractTargetLanguage
from target.targetlanguage import CommonInfixFormatter
from target.targetlanguage import NodeVisitor
import ast
import asttoken
import context
import copy
import nodeattrs
import nodebuilder
import templates


EXPLICIT_TYPE_DECLARATION = "golang__explicit_type_decl"
EXPLICIT_TYPE_DECLARATION_NULL_RHS = "golang__explicit_type_decl_rhs"


class GolangTypeDeclarationTemplate(templates.TypeDeclarationTemplate):

    def __init__(self):
        super().__init__("$identifier := ")

    def pre_render__hook(self, declaration, scope, node_attrs):
        if EXPLICIT_TYPE_DECLARATION_NULL_RHS in node_attrs:
            return "var $identifier $type"
        else:
            return declaration


class GolangSyntax(AbstractTargetLanguage):

    def __init__(self):
        super().__init__(formatter=GolangFormatter(),
                         is_prefix=False,
                         stmt_end_delim=";", stmt_end_delim_always_required=False,
                         block_start_delim="{", block_end_delim="}",
                         flow_control_test_start_delim="",
                         flow_control_test_end_delim="",
                         loop_foreach_keyword="",
                         arg_delim=",",
                         explicit_rtn=True,
                         has_block_scope=True,
                         has_assignment_lhs_unpacking=False,
                         type_declaration_template=GolangTypeDeclarationTemplate(),
                         function_signature_template="func $func_name($args_start$arg_name $arg_type, $args_end) $rtn_type",
                         function_can_return_multiple_values=True,
                         has_pointers=True)

        self.type_mapper.register_none_type_name("nil")
        self.type_mapper.register_simple_type_mapping(bool,"bool", lambda v: "true" if v else "false")
        self.type_mapper.register_simple_type_mapping(int, "int")
        self.type_mapper.register_simple_type_mapping(str, "string")
        self.type_mapper.register_simple_type_mapping(float, "float32")

        self.type_mapper.register_container_type_mapping(
            list,
            "[]$contained_type$[0]",
            start_literal="[]$contained_type$[0]{",
            end_literal="}")

        # TODO - review best way to represent a tuple in Golang
        self.type_mapper.register_container_type_mapping(
            tuple,
            "[]Tuple",
            start_literal="[]$contained_type$[]{",
            end_literal="}")

        # homogeneous tuple - make this a slice
        self.type_mapper.register_container_type_mapping(
            tuple,
            "[]$contained_type$[0]",
            start_literal="[]$contained_type$[0]{",
            end_literal="}",
            apply_if=lambda type_info: type_info.contains_homogeneous_types)

        map_decl = "map[$contained_type$[0]]$contained_type$[1]"
        self.type_mapper.register_container_type_mapping(
            dict,
            map_decl,
            start_literal="%s{" % map_decl,
            end_literal="}",
            values_separator=":")

        self.type_mapper.register_type_coercion_rule(str, int, str, "string")
        self.type_mapper.register_type_coercion_rule(str, float, str, "string")

        def _visit_assignment(args, rw):
            rhs_arg = args[1]
            if rhs_arg.type is type(None):
                # this case doesnt' work:
                # i = None
                # i = 1
                # i = None
                # ... unless we make every type a pointer...
                nodeattrs.set_attr(rw.node, EXPLICIT_TYPE_DECLARATION_NULL_RHS)
                setattr(rhs_arg.node, nodeattrs.SKIP_NODE_ATTR, True)
                
        self.register_function_rewrite(py_name="<>_=", py_type=None,
                                       rewrite=_visit_assignment)

        def _rewrite_str_mod(args, rw):
            format_call = rw.call("fmt.Sprintf")
            keep_args = True
            rhs = args[1]
            if rhs.type is tuple:
                keep_args = False
                format_call.append_arg(args[0])
                format_call.append_args(rhs.node.elts)
            rw.replace_node_with(format_call, keep_args)
        self.register_function_rewrite(
            py_name="<>_%", py_type=str,
            rewrite=_rewrite_str_mod)

        self.register_function_rewrite(
            py_name="<>_loop_for", py_type=None, rewrite=lambda args, rw:
                rw.rewrite_as_c_style_loop())

        self.register_function_rename(py_name="print", py_type=None,
                                      target_name="fmt.Println")

        # str
        self.register_function_rewrite(
            py_name="startswith", py_type=str, target_name="strings.HasPrefix",
            rewrite=lambda args, rw: rw.rewrite_as_func_call(inst_1st=True))
        
        self.register_function_rewrite(
            py_name="endswith", py_type=str, target_name="strings.HasSuffix",
            rewrite=lambda args, rw: rw.rewrite_as_func_call(inst_1st=True))

        self.register_function_rewrite(
            py_name="strip", py_type=str, target_name="strings.TrimSpace",
            rewrite=lambda args, rw: rw.rewrite_as_func_call())

        self.register_function_rewrite(
            py_name="join", py_type=str, target_name="strings.Join",
            rewrite=lambda args, rw: rw.rewrite_as_func_call(inst_1st=False))

        self.register_function_rewrite(
            py_name="split", py_type=str, target_name="strings.Split",
            rewrite=lambda args, rw:
                # python has split(), which splits in whitespace
                rw.rewrite_as_func_call(inst_1st=True).append_arg(" ")
                if len(args) == 0 else rw.rewrite_as_func_call(inst_1st=True))

        self.register_function_rewrite(
            py_name="index", py_type=str, target_name="strings.Index",
            rewrite=lambda args, rw: rw.rewrite_as_func_call(inst_1st=True))

        self.register_function_rewrite(
            py_name="find", py_type=str, target_name="strings.Index",
            rewrite=lambda args, rw: rw.rewrite_as_func_call(inst_1st=True))

        def _slice_rewrite(args, rw):
            if len(args) == 2 and isinstance(args[1].node, ast.UnaryOp):
                lhs = nodebuilder.call("len", [rw.target_node])
                rhs = args[1].node.operand
                binop = nodebuilder.binop("-", lhs, rhs)
                setattr(args[1].node, nodeattrs.ALT_NODE_ATTR, binop)
        self.register_function_rewrite(
            py_name="<>_[]", py_type=str,
            rewrite=_slice_rewrite)

        # input
        self.register_function_rewrite(
            py_name="input", py_type=str,
            target_name="bufio.NewReader(os.Stdin).ReadString",
            rewrite=lambda args, rw:
                rw.insert_above(rw.call(context.PRINT_BUILTIN).append_arg(args[0]))
                  .replace_args_with('\\n'))

        # list
        self.register_function_rewrite(
            py_name="append", py_type=list,
            rewrite=lambda args, rw: rw
                .rewrite_as_func_call(inst_1st=True)
                .reassign_to_arg())

        # file
        self.type_mapper.register_simple_type_mapping(context.TypeInfo.textiowraper(), "os.File")
        self.register_function_rename(
            py_name="open", py_type=str, target_name="os.Open")

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
        return super().delim_suffix(token, remaining_tokens)
