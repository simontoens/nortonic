from target.targetlanguage import AbstractLanguageFormatter
from target.targetlanguage import AbstractTargetLanguage
import ast
import asttoken
import context
import functools
import nodebuilder
import templates


class ElispFunctionSignatureTemplate(templates.FunctionSignatureTemplate):

    def __init__(self):
        super().__init__("(defun $func_name ($args_start$arg_name $args_end)")

    def get_function_body_end_delim(self):
        """
        This is here because function declarations are not re-written below
        as function calls (...anymore, they used to be, but that caused other
        issues because if everything is just a function call, we can't do
        anything useful with scope pushing - maybe that's actually ok for elisp,
        but it was making code more messy downstream - btw the plane is just
        flying over Greenland)
        """
        return ")"


class ElispSyntax(AbstractTargetLanguage):

    def __init__(self):
        super().__init__(formatter=ElispFormatter(),
                         is_prefix=True,
                         stmt_end_delim="",
                         block_start_delim="", block_end_delim="",
                         flow_control_test_start_delim="", flow_control_test_end_delim="",
                         arg_delim=" ",
                         strongly_typed=False,
                         explicit_rtn=False,
                         has_block_scope=False,
                         has_assignment_lhs_unpacking=False,
                         function_signature_template=ElispFunctionSignatureTemplate())
        
        self.type_mapper.register_none_type_name("nil")
        self.type_mapper.register_simple_type_mapping(bool, None, lambda v: "t" if v else "nil")
        self.type_mapper.register_container_type_mapping((list, tuple), "list", "(list", ")")
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

        self.register_function_rename(py_name="input", py_type=None, target_name="read-string")

        self.register_function_rewrite(
            py_name="<>_+", py_type=None,
            rewrite=lambda args, rw:
                rw.replace_node_with(rw.call("concat"))
                if args[0].type is str else rw.replace_node_with(rw.call("+")))

        self.register_function_rewrite(
            py_name="<>_-", py_type=None,
            rewrite=lambda args, rw: rw.replace_node_with(rw.call("-")))

        self.register_function_rewrite(
            py_name="<>_unary-", py_type=None,
            rewrite=lambda args, rw:
                rw.replace_node_with(rw.call("-")) if isinstance (args[0].node, ast.Call) else None)

        self.register_function_rewrite(
            py_name="<>_*", py_type=None,
            rewrite=lambda args, rw: rw.replace_node_with(rw.call("*")))

        self.register_function_rewrite(
            py_name="<>_/", py_type=None,
            rewrite=lambda args, rw: rw.replace_node_with(rw.call("/")))

        def _aug_assign_rewrite(op, args, rw):
            if op == "+" and args[0].type is str:
                op = "concat"
            rw.replace_node_with(rw.call(op)).reassign_to_arg()

        self.register_function_rewrite(
            py_name="<>_=_aug_+", py_type=None,
            rewrite=functools.partial(_aug_assign_rewrite, "+"))

        self.register_function_rewrite(
            py_name="<>_=_aug_-", py_type=None,
            rewrite=functools.partial(_aug_assign_rewrite, "-"))

        self.register_function_rewrite(
            py_name="<>_=_aug_*", py_type=None,
            rewrite=functools.partial(_aug_assign_rewrite, "*"))

        self.register_function_rewrite(
            py_name="<>_=_aug_/", py_type=None,
            rewrite=functools.partial(_aug_assign_rewrite, "/"))

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
            is_counting_loop = rw.is_range_loop() or rw.is_enumerated_loop()
            if is_counting_loop:
                # rewrite as for i = 0; i < ...
                rw.rewrite_as_c_style_loop()
                # for i in range(...)
                init_node = rw.get_for_loop_init_node()
                cond_node = rw.get_for_loop_cond_node()
                expr_node = rw.get_for_loop_expr_node()
                expr_node_value = expr_node.value
                count_is_negative = isinstance(expr_node_value, ast.UnaryOp) and isinstance(expr_node_value.op, ast.USub)
                if count_is_negative:
                    from_keyword = "downfrom"
                    # the value following "by" should not be negative
                    expr_node_value = expr_node_value.operand
                else:
                    from_keyword = "from"

                # from/downfrom are inclusive, the end value in range is not,
                # if we have a constant, adjust by 1
                end_value_node = cond_node.comparators[0]
                if isinstance(end_value_node, ast.Constant):
                    end_value = end_value_node.value + (1 if count_is_negative else -1)
                    end_value_node = nodebuilder.constant(end_value)
                else:
                    op = "+" if count_is_negative else "-"
                    end_value_node = rw.binop(op, end_value_node, 1)

                f = rw.call("cl-loop")\
                    .append_arg(rw.ident("for"))\
                    .append_arg(init_node.targets[0])\
                    .append_arg(rw.ident(from_keyword))\
                    .append_arg(init_node.value)\
                    .append_arg(rw.ident("to"))\
                    .append_arg(end_value_node)\
                    .append_arg(rw.ident("by"))\
                    .append_arg(expr_node_value)\
                    .append_arg(rw.ident("do"))
            else:
                # for item in my_list ...
                target_node = args[0].node
                f = rw.call("dolist")
                args_list = rw.call(target_node.id).append_arg(args[1].node)
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

        self.register_function_rewrite(
            py_name="<>_less_than", py_type=None,
            rewrite=lambda args, rw: rw.replace_node_with(rw.call("<")))

        # str
        self.register_function_rewrite(
            py_name="startswith", py_type=str, target_name="string-prefix-p",
            rewrite=lambda args, rw: rw.rewrite_as_func_call())
        
        self.register_function_rewrite(
            py_name="endswith", py_type=str, target_name="string-suffix-p",
            rewrite=lambda args, rw: rw.rewrite_as_func_call())

        self.register_function_rewrite(
            py_name="join", py_type=str, target_name="mapconcat",
            rewrite=lambda args, rw: rw.rewrite_as_func_call().prepend_arg(rw.ident("'identity")))

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
            py_name="<>_[]", py_type=tuple,
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
        if asttoken.next_token_has_type(remaining_tokens, asttoken.CUSTOM_FUNCDEF_END_BODY_DELIM):
            # removes the last space:
            # (defun foo
            #    (list 1 "hello" 1.2) )
            return False
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
