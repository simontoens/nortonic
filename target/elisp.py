from target.targetlanguage import AbstractLanguageFormatter
from target.targetlanguage import AbstractTargetLanguage
import asttoken
import context
import functools


class ElispSyntax(AbstractTargetLanguage):

    def __init__(self):
        super().__init__(formatter=ElispFormatter(),
                         is_prefix=True,
                         stmt_start_delim="", stmt_end_delim="",
                         block_start_delim="", block_end_delim="",
                         flow_control_test_start_delim="", flow_control_test_end_delim="",
                         arg_delim=" ",
                         strongly_typed=False,
                         explicit_rtn=False,
                         has_block_scope=False,
                         has_assignment_lhs_unpacking=False,
                         function_signature_template=None)

        self.type_mapper.register_none_type_name("nil")        
        self.type_mapper.register_simple_type_mapping(bool, None, lambda v: "t" if v else "nil")
        self.type_mapper.register_container_type_mapping(list, "list", "(list", ")")
        self.type_mapper.register_container_type_mapping(tuple, "list", "(list", ")")
        self.type_mapper.register_container_type_mapping(dict, "hash-table", "#s(hash-table test equal data (", "))")
        self.type_mapper.register_type_coercion_rule(str, int, str, "int-to-string")
        self.type_mapper.register_type_coercion_rule(str, float, str, "int-to-string")

        self.register_function_rewrite(
            py_name="append", py_type=list,
            target_name="add-to-list",
            rewrite=lambda args, rw: rw
                .rewrite_as_func_call(inst_1st=True, inst_renamer=lambda v: "'" + v))

        self.register_function_rewrite(
            py_name="<>_=", py_type=None,
            rewrite=lambda args, rw: rw.replace_node_with(rw.call("setq")))
        self.register_function_rewrite(
            py_name="print", py_type=None,
            target_name="message",
            rewrite=lambda args, rw:
                rw.prepend_arg(" ".join(["%s" for a in args]))
                if len(args) > 1 or (len(args) == 1 and args[0].type != str) else None)
        self.register_function_rewrite(
            py_name="<>_+", py_type=None,
            rewrite=lambda args, rw:
                rw.replace_node_with(rw.call("concat"))
                if args[0].type == str else rw.replace_node_with(rw.call("+")))

        self.register_function_rewrite(
            py_name="<>_*", py_type=None,
            rewrite=lambda args, rw:
                rw.replace_node_with(rw.call("*")))

        self.register_function_rewrite(
            py_name="<>_/", py_type=None,
            rewrite=lambda args, rw:
                rw.replace_node_with(rw.call("/")))

        self.register_function_rewrite(
            py_name="<>_-", py_type=None,
            rewrite=lambda args, rw:
                rw.replace_node_with(rw.call("-")))

        def _rewrite_str_mod(args, rw):
            format_call = rw.call("format")
            keep_args = True
            rhs = args[1]
            if rhs.type is tuple or rhs.type is list:
                # testing for list should't be required, this is a bug
                keep_args = False
                format_call.append_arg(args[0])
                format_call.append_args(rhs.node.elts)
            rw.replace_node_with(format_call, keep_args)
        self.register_function_rewrite(
            py_name="<>_%", py_type=str,
            rewrite=_rewrite_str_mod)

        self.register_function_rewrite(
            py_name="<>_%", py_type=None,
            rewrite=lambda args, rw:
                rw.replace_node_with(rw.call("mod")))

        self.register_function_rewrite(
            py_name="<>_&&", py_type=None,
            rewrite=lambda args, rw:
                rw.replace_node_with(rw.call("and")))

        self.register_function_rewrite(
            py_name="<>_||", py_type=None,
            rewrite=lambda args, rw:
                rw.replace_node_with(rw.call("or")))

        def _defun_rewrite(args, rw):
            f = rw.call("defun")
            f.prepend_arg(rw.ident(rw.node.name))
            if len(args) == 0:
                args_list = rw.call("")
            else:
                args_list = rw.call(args[0].node.arg)
                for i, arg in enumerate(args):
                    if i > 0:
                        args_list.append_arg(rw.ident(arg.node.arg))
            f.append_arg(args_list)
            f.append_to_body(rw.node.body)
            rw.replace_node_with(f, keep_args=False)
        self.register_function_rewrite(py_name="<>_funcdef", py_type=None, rewrite=_defun_rewrite)

        def _if_rewrite(args, rw):
            if_func = rw.call("if")
            assert len(rw.node.body) >= 1
            if_block_has_single_stmt = len(rw.node.body) == 1
            else_block_exists = len(rw.node.orelse) > 0
            if if_block_has_single_stmt:
                if_func.append_to_body(rw.node.body)
            else:
                progn = rw.call("progn")
                progn.append_to_body(rw.node.body)
                if_func.append_to_body(progn.node)
            if else_block_exists:
                if_func.append_to_body(rw.node.orelse)
            rw.replace_node_with(if_func)
        self.register_function_rewrite(py_name="<>_if", py_type=None, rewrite=_if_rewrite)

        def _for_rewrite(args, rw):
            f = rw.call("dolist")
            args_list = rw.call(args[0].node.id).append_arg(args[1].node)
            f.append_arg(args_list)
            f.append_to_body(rw.node.body)
            rw.replace_node_with(f, keep_args=False)
        self.register_function_rewrite(py_name="<>_loop_for", py_type=None, rewrite=_for_rewrite)        

        self.register_function_rewrite(
            py_name="<>_==", py_type=None,
            rewrite=lambda args, rw: rw.replace_node_with(rw.call("equal")))

        self.register_function_rewrite(
            py_name="<>_is", py_type=None,
            rewrite=lambda args, rw: rw.replace_node_with(rw.call("eq")))

        # str
        self.register_function_rewrite(
            py_name="endswith", py_type=str, target_name="string-suffix-p",
            rewrite=lambda args, rw: rw.rewrite_as_func_call())

        self.register_function_rewrite(
            py_name="join", py_type=str, target_name="mapconcat",
            rewrite=lambda args, rw: rw.rewrite_as_func_call().prepend_arg(rw.ident("'identity")))

        self.register_function_rewrite(
            py_name="startswith", py_type=str, target_name="string-prefix-p",
            rewrite=lambda args, rw: rw.rewrite_as_func_call())

        self.register_function_rewrite(
            py_name="strip", py_type=str, target_name="string-trim",
            rewrite=lambda args, rw: rw.rewrite_as_func_call())

        self.register_function_rewrite(
            py_name="upper", py_type=str, target_name="upcase",
            rewrite=lambda args, rw: rw.rewrite_as_func_call())

        self.register_function_rewrite(
            py_name="lower", py_type=str, target_name="downcase",
            rewrite=lambda args, rw: rw.rewrite_as_func_call())

        self.register_function_rewrite(
            py_name="split", py_type=str, target_name="split-string",
            rewrite=lambda args, rw: rw.rewrite_as_func_call(inst_1st=True))

        self.register_function_rewrite(
            py_name="index", py_type=str, target_name="cl-search",
            rewrite=lambda args, rw: rw.rewrite_as_func_call(inst_1st=False))

        self.register_function_rewrite(
            py_name="find", py_type=str, target_name="cl-search",
            rewrite=lambda args, rw: rw.rewrite_as_func_call(inst_1st=False))

        self.register_function_rewrite(
            py_name="<>_[]", py_type=str,
            rewrite=lambda args, rw: rw.call_with_target_as_arg("substring"))

        # file
        self.register_function_rewrite(py_name="open", py_type=str,
            rewrite=lambda args, rw: rw.replace_node_with(rw.wrap(args[0].node)))
        def _read_rewrite(args, rw, is_readlines):
            rw.rewrite_as_func_call()            
            f = rw.call("with-temp-buffer")\
                .append_to_body(rw.call("insert-file-contents")
                    .append_args(rw.arg_nodes))\
                .append_to_body(rw.call("buffer-string"))
            if is_readlines:
                f = rw.call("split-string").append_to_body(f, "\\n")
            rw.replace_node_with(f, keep_args=False)

        self.register_function_rewrite(
            py_name="read", py_type=context.TypeInfo.textiowraper(),
            rewrite=functools.partial(_read_rewrite, is_readlines=False))

        self.register_function_rewrite(
            py_name="readlines", py_type=context.TypeInfo.textiowraper(),
            rewrite=functools.partial(_read_rewrite, is_readlines=True))

        self.register_function_rewrite(py_name="write", py_type=context.TypeInfo.textiowraper(),
            rewrite=lambda args, rw:
                rw.rewrite_as_func_call()
                    .replace_node_with(
                        rw.call("with-temp-file")
                            .append_arg(rw.arg_nodes[1])
                            .append_to_body(rw.call("insert").append_arg(rw.arg_nodes[0])),
                        keep_args=False))

        # list
        self.register_function_rename(py_name="len", py_type=None, target_name="length")

        self.register_function_rewrite(
            py_name="<>_[]", py_type=list,
            rewrite=lambda args, rw:
                rw.call_with_target_as_arg("nth", target_as_first_arg=False))

        self.register_function_rewrite(
            py_name="sort", py_type=list,
            rewrite=lambda args, rw:
                rw.rewrite_as_func_call().append_arg(rw.ident("'<")).reassign_to_arg())

        # dict
        self.register_function_rewrite(
            py_name="<>_[]", py_type=dict,
            rewrite=lambda args, rw:
                rw.call_with_target_as_arg("gethash", target_as_first_arg=False))

        self.register_function_rewrite(
            py_name="<>_dict_assignment", py_type=dict,
            rewrite=lambda args, rw:
                rw.call_with_target_as_arg("puthash", target_as_first_arg=False))
        # os
        self.register_attribute_rewrite(
            py_name="sep", py_type=context.TypeInfo.module("os"),
            rewrite=lambda args, rw: rw.replace_node_with(rw.const("/")))

        # os.path
        self.register_attribute_rewrite(
            py_name="sep", py_type=context.TypeInfo.module("os.path"),
            rewrite=lambda args, rw: rw.replace_node_with(rw.const("/")))

        # this requires f.el https://github.com/rejeep/f.el
        self.register_function_rewrite(
            py_name="join", py_type=context.TypeInfo.module("os.path"),
            rewrite=lambda args, rw: rw.replace_node_with(rw.call("f-join")))


class ElispFormatter(AbstractLanguageFormatter):

    def delim_suffix(self, token, remaining_tokens):
        if token.type.is_func_call_boundary and token.is_start:
            # no space after '('
            return False
        if token.type.is_container_literal_boundary and token.is_start:
            if token.value == "(list":
                if asttoken.next_token_has_type(remaining_tokens, asttoken.CONTAINER_LITERAL_BOUNDARY):
                    # an empty list doesn't need a space: (list)
                    return False
                else:
                    # for (list <item>) we want a space after "(list": (list 1)
                    return True
            else:
                return False
        if asttoken.is_boundary_ending_before_value_token(
                remaining_tokens, asttoken.FUNC_CALL_BOUNDARY):
            # no space if next token is ')'
            return False
        if asttoken.is_boundary_ending_before_value_token(
                remaining_tokens, asttoken.CONTAINER_LITERAL_BOUNDARY):
            # no space if next token is ')'
            return False
        return super().delim_suffix(token, remaining_tokens)

    def newline(self, token, remaining_tokens):
        if asttoken.is_boundary_ending_before_value_token(
                remaining_tokens, asttoken.FUNC_CALL_BOUNDARY):
            return False
        if token.type.is_block and token.is_end:
            return True
        if token.type.is_stmt and token.is_end:
            return True
        return super().newline(token, remaining_tokens)
