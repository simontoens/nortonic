from target.targetlanguage import AbstractTargetLanguage
from target.targetlanguage import CommonInfixFormatter
import ast
import asttoken
import context
import functools
import nodebuilder


class JavaSyntax(AbstractTargetLanguage):

    def __init__(self):
        super().__init__(formatter=JavaFormatter(),
                         is_prefix=False,
                         stmt_start_delim="", stmt_end_delim=";",
                         block_start_delim="{", block_end_delim="}",
                         flow_control_test_start_delim="(", flow_control_test_end_delim=")",
                         equality_binop = "==", identity_binop="==",
                         and_binop="&&", or_binop="||",
                         loop_foreach_keyword=":",
                         arg_delim=",",
                         strongly_typed=True,
                         explicit_rtn=True,
                         has_block_scope=True,
                         has_assignment_lhs_unpacking=False,
                         ternary_replaces_if_expr=True,
                         function_signature_template="$visibility $rtn_type $func_name($args_start$arg_type $arg_name, $args_end)")

        self.type_mapper.register_none_type_name("null")
        self.type_mapper.register_simple_type_mapping(int,  "Integer")
        self.type_mapper.register_simple_type_mapping(float,  "Float")
        self.type_mapper.register_simple_type_mapping(str,  "String")
        self.type_mapper.register_simple_type_mapping(bool, "Boolean", lambda v: "true" if v else "false")

        self.type_mapper.register_container_type_mapping(
            list,
            "List<?>",
            "new ArrayList<>(List.of(", "))")
        self.type_mapper.register_container_type_mapping(
            tuple,
            "Tuple<?>",
            "Tuple.of(", ")")
        self.type_mapper.register_container_type_mapping(
            dict,
            "Map<?>",
            "new HashMap<>(Map.of(", "))", ",")

        print_fmt = {int: "%d", float: "%d", str: "%s"}
        self.register_function_rewrite(
            py_name="print", py_type=None,
            target_name="System.out.println",
            rewrite=lambda args, rw:
                rw.replace_args_with(
                  rw.call("String.format")
                    .prepend_arg(" ".join([print_fmt[a.type] for a in args]))
                      .append_args(args))
                if len(args) > 1 else None)

        self.register_function_rewrite(
            py_name="len", py_type=str,
            target_name="length",
            rewrite=lambda args, rw:
                rw.rewrite_as_attr_method_call())

        self.register_function_rewrite(
            py_name="len", py_type=list,
            target_name="size",
            rewrite=lambda args, rw:
                rw.rewrite_as_attr_method_call())

        self.register_function_rewrite(
            py_name="len", py_type=tuple,
            target_name="size",
            rewrite=lambda args, rw:
                rw.rewrite_as_attr_method_call())

        self.register_function_rewrite(
            py_name="<>_==", py_type=None,
            rewrite=lambda args, rw:
                rw.replace_node_with(rw.call("equals"))
                  .rewrite_as_attr_method_call() # equals(s s2) -> s.equals(s2)
                if args[0].type == str else None) # only for str...for now FIX

        # str
        self.register_function_rename(py_name="endswith", py_type=str,
                                      target_name="endsWith")
        self.register_function_rename(py_name="startswith", py_type=str,
                                      target_name="startsWith")
        self.register_function_rename(py_name="strip", py_type=str,
                                      target_name="trim")
        self.register_function_rename(py_name="upper", py_type=str,
                                      target_name="toUpperCase")
        self.register_function_rename(py_name="lower", py_type=str,
                                      target_name="toLowerCase")
        self.register_function_rename(py_name="index", py_type=str,
                                      target_name="indexOf")
        self.register_function_rename(py_name="find", py_type=str,
                                      target_name="indexOf")

        self.register_function_rewrite(
            py_name="join", py_type=str, target_name="String.join",
            rewrite=lambda args, rw:
                rw.rewrite_as_func_call(inst_1st=True))

        def _split_rewrite(args, rw):
            if len(args) == 0:
                # python has split(), which splits in whitespace
                rw.append_arg(" ")
            rw.replace_node_with(rw.call("Arrays.asList"),
                                 current_node_becomes_singleton_arg=True)
        self.register_function_rewrite(
            py_name="split", py_type=str, rewrite=_split_rewrite)

        def _slice_rewrite(args, rw):
            if len(args) == 2 and isinstance(args[1].node, ast.UnaryOp):
                lhs = nodebuilder.attr_call(rw.target_node, "length")
                rhs = args[1].node.operand
                binop = nodebuilder.binop("-", lhs, rhs)
                rw.call_on_target("substring", keep_args=False).append_arg(args[0]).append_arg(binop)
            else:
                rw.call_on_target("substring")
        self.register_function_rewrite(
            py_name="<>_[]", py_type=str,
            rewrite=_slice_rewrite)

        # file
        self.type_mapper.register_simple_type_mapping(context.TypeInfo.textiowraper(), "File")
        self.register_function_rewrite(
            py_name="open", py_type=str, target_name="new File",
            rewrite=lambda args, rw: rw.keep_first_arg())

        def _read_rewrite(args, rw, is_readlines):
            rw.rewrite_as_func_call().rename("Files.readString")
            file_arg = rw.wrap(rw.arg_nodes[0])
            rw.replace_args_with(file_arg.chain_method_call("toPath"))
            if is_readlines:
                # in python readlines returns a list of strings
                # so we'll call split("\n")
                rw.chain_method_call("split", args=["\\n"])
                # split returns an Array, so we wrap the whole thing in
                # Arrays.asList
                rw.replace_node_with(rw.call("Arrays.asList"),
                                     current_node_becomes_singleton_arg=True)

        self.register_function_rewrite(
            py_name="read", py_type=context.TypeInfo.textiowraper(),
            rewrite=functools.partial(_read_rewrite, is_readlines=False))

        self.register_function_rewrite(
            py_name="readlines", py_type=context.TypeInfo.textiowraper(),
            rewrite= functools.partial(_read_rewrite, is_readlines=True))

        def _write_rewrite(args, rw):
            rw.rewrite_as_func_call().rename("Files.writeString")
            content_arg = rw.arg_nodes[0]
            file_arg = rw.wrap(rw.arg_nodes[1])
            rw.replace_args_with(
                file_arg.chain_method_call("toPath"))\
                .append_arg(content_arg)\
                .append_arg(rw.ident("Charset.defaultCharset()"))
        self.register_function_rewrite(
            py_name="write", py_type=context.TypeInfo.textiowraper(),
            rewrite=_write_rewrite)


        # list
        self.register_function_rename(py_name="append", py_type=list,
                                      target_name="add")

        self.register_function_rewrite(
            py_name="sort", py_type=list,
            rewrite=lambda args, rw:
                rw.append_arg(rw.ident("null")))

        self.register_function_rewrite(
            py_name="<>_[]", py_type=list,
            rewrite=lambda args, rw: rw.call_on_target("get"))

        # dict
        self.register_function_rewrite(
            py_name="<>_[]", py_type=dict,
            rewrite=lambda args, rw: rw.call_on_target("get"))

        self.register_function_rewrite(
            py_name="<>_dict_assignment", py_type=dict,
            rewrite=lambda args, rw: rw.call_on_target("put"))


        # os

        # os.sep is the same as os.path.sep but Java is also a respectable
        # language with different ways of getting at the path sep
        self.register_attribute_rewrite(
            py_name="sep", py_type=context.TypeInfo.module("os"),
            rewrite=lambda args, rw: rw.replace_node_with(rw.call("System.getProperty").append_arg("file.separator")))

        # os.path

        # os.sep is the same as os.path.sep but Java is also a respectable
        # language with different ways of getting at the path sep
        self.register_attribute_rewrite(
            py_name="sep", py_type=context.TypeInfo.module("os.path"),
            rewrite=lambda args, rw: rw.replace_node_with(rw.ident("File.separator")))

        self.register_function_rewrite(
            py_name="join", py_type=context.TypeInfo.module("os.path"),
            rewrite=lambda args, rw: rw.replace_node_with(rw.call("Paths.get")).chain_method_call("toString"))


class JavaFormatter(CommonInfixFormatter):

    def delim_suffix(self, token, remaining_tokens):
        if (asttoken.is_boundary_ending_before_value_token(
                remaining_tokens, asttoken.STMT) or asttoken.is_boundary_ending_before_value_token(
                remaining_tokens, asttoken.BODY_STMT)):
            # we want foo; not foo ;
            return False
        if asttoken.is_boundary_ending_before_value_token(
                remaining_tokens, asttoken.FLOW_CONTROL_TEST):
            # we want if (1 == 1), not if (1 == 1 )
            return False
        if asttoken.is_boundary_starting_before_value_token(
                remaining_tokens, asttoken.BLOCK):
            # we want if (1 == 1) {, not if (1 == 1){
            return True
        if token.type.is_block and token.is_end:
            # we want } else, not }else
            return True
        return super().delim_suffix(token, remaining_tokens)

    def newline(self, token, remaining_tokens):
        if token.type.is_block and token.is_end:
            if asttoken.next_token_has_type(remaining_tokens, asttoken.KEYWORD_ELSE):
                return False
        return super().newline(token, remaining_tokens)
