import ast
import functools
import lang.internal.typeinfo as ti
import lang.nodebuilder as nodebuilder
import lang.target.rewrite as rewrite
import lang.target.targetlanguage as targetlanguage
import lang.target.templates as templates
import os
import types
import visitor.asttoken as asttoken
import visitor.nodeattrs as nodeattrs


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


class ElispSyntax(targetlanguage.AbstractTargetLanguage):

    def __init__(self):
        super().__init__(formatter=ElispFormatter(),
                         is_prefix=True,
                         arg_delim=" ",
                         dynamically_typed=True,
                         explicit_rtn=False,
                         has_if_expr=True,
                         has_block_scope=False,
                         has_assignment_lhs_unpacking=False,
                         function_signature_template=ElispFunctionSignatureTemplate())
        
        self.type_mapper.register_none_type_name("nil")
        self.type_mapper.register_simple_type_mapping(bool, None, lambda v: "t" if v else "nil")
        self.type_mapper.register_container_type_mapping((list, tuple), "list", "(list", ")")
        self.type_mapper.register_container_type_mapping(dict, "hash-table", "#s(hash-table test equal data (", "))")
        self.type_mapper.register_type_coercion_rule(str, int, str, "int-to-string")
        self.type_mapper.register_type_coercion_rule(str, float, str, "int-to-string")

        self.register_rewrite(list.append, rename_to="add-to-list",
            rewrite=lambda args, rw: rw
                .rewrite_as_func_call(inst_1st=True, inst_node_attrs=nodeattrs.QUOTE_NODE_ATTR))

        self.register_rewrite(rewrite.Operator.ASSIGNMENT,
            rewrite=lambda args, rw: rw.replace_node_with(rw.call("setq")))

        self.register_rewrite(print, rename_to="message",
            rewrite=lambda args, rw:
                rw.prepend_arg(" ".join(["%s" for a in args]))
                if len(args) > 1 or (len(args) == 1 and args[0].type is not str) else None)

        self.register_rename("input", to="read-string")

        self.register_rewrite(rewrite.Operator.ADD, rewrite=lambda args, rw:
            rw.replace_node_with(rw.call("concat"))
            if args[0].type is str else rw.replace_node_with(rw.call("+")))

        self.register_rewrite(rewrite.Operator.SUB, rewrite=lambda args, rw:
            rw.replace_node_with(rw.call("-")))

        self.register_rewrite(rewrite.Operator.U_SUB, rewrite=lambda args, rw:
            rw.replace_node_with(rw.call("-")) if isinstance (args[0].node, ast.Call) else None)

        self.register_rewrite(rewrite.Operator.MULT, rewrite=lambda args, rw:
            rw.replace_node_with(rw.call("*")))

        self.register_rewrite(rewrite.Operator.DIV, rewrite=lambda args, rw:
            rw.replace_node_with(rw.call("/")))

        def _aug_assign_rewrite(op, args, rw):
            if op == "+" and args[0].type is str:
                op = "concat"
            rw.replace_node_with(rw.call(op)).reassign_to_arg()

        self.register_rewrite(rewrite.Operator.AUG_ADD,
            rewrite=functools.partial(_aug_assign_rewrite, "+"))

        self.register_rewrite(rewrite.Operator.AUG_SUB,
            rewrite=functools.partial(_aug_assign_rewrite, "-"))

        self.register_rewrite(rewrite.Operator.AUG_MULT,
            rewrite=functools.partial(_aug_assign_rewrite, "*"))

        self.register_rewrite(rewrite.Operator.AUG_DIV,        
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
        self.register_rewrite(rewrite.Operator.MOD, arg_type=str,
                              rewrite=_rewrite_str_mod)

        self.register_rewrite(rewrite.Operator.MOD,
            rewrite=lambda args, rw: rw.replace_node_with(rw.call("mod")))

        self.register_rewrite(rewrite.Operator.AND, rewrite=lambda args, rw:
                              rw.replace_node_with(rw.call("and")))

        self.register_rewrite(rewrite.Operator.OR, rewrite=lambda args, rw:
                              rw.replace_node_with(rw.call("or")))

        def _if_rewrite(args, rw, is_expr):
            if is_expr:
                # make the ast structure the same as a regular if-stmt
                # so that we can use the same logic below for both
                if isinstance(rw.node.body, ast.AST):
                    rw.node.body = [rw.node.body]
                if isinstance(rw.node.orelse, ast.AST):
                    rw.node.orelse = [rw.node.orelse]

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
        self.register_rewrite(rewrite.Keyword.IF,
            rewrite=functools.partial(_if_rewrite, is_expr=False))
        self.register_rewrite(rewrite.Keyword.IF_EXPR,
            rewrite=functools.partial(_if_rewrite, is_expr=True))

        def _for_rewrite(args, rw):
            is_counting_loop = rw.is_range_loop() or rw.is_enumerated_loop()
            if is_counting_loop:
                # rewrite as for i = 0; i < ...
                rw.rewrite_as_c_style_loop()
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
                    end_value_node = rw.binop(op, end_value_node, 1, rhs_type=int)

                counter_node = init_node.targets[0]
                # attach additional md to this counter node:
                # it is a declaration identifier
                # its type (so that the type visitor can find it again)
                counter_node = rw.ident(
                    counter_node,
                    ti.TypeInfo.int(),
                    node_attrs={nodeattrs.ASSIGN_LHS_NODE_ATTR: counter_node})
                f = rw.call("cl-loop")\
                    .append_arg(rw.xident("for"))\
                    .append_arg(counter_node)\
                    .append_arg(rw.xident(from_keyword))\
                    .append_arg(init_node.value)\
                    .append_arg(rw.xident("to"))\
                    .append_arg(end_value_node)\
                    .append_arg(rw.xident("by"))\
                    .append_arg(expr_node_value)\
                    .append_arg(rw.xident("do"))                
            else:
                # for item in my_list ...
                target_node = args[0].node
                f = rw.call("dolist")
                # this is weird, but the syntax is:
                # (dolist (s some-list) <- s is the target_node
                # so we need rw.call(target_node)
                # additionally, we need to mark this node as a decl node:
                node_md={nodeattrs.ASSIGN_LHS_NODE_ATTR: target_node}
                args_list = rw.call(target_node, node_metadata=node_md).append_arg(args[1].node)
                f.append_arg(args_list)
            f.append_to_body(rw.node.body)
            rw.replace_node_with(f, keep_args=False)
        self.register_rewrite(rewrite.Keyword.FOR, rewrite=_for_rewrite)

        self.register_rewrite(rewrite.Operator.EQUALS,
            rewrite=lambda args, rw: rw.replace_node_with(rw.call("equal")))

        self.register_rewrite(rewrite.Operator.NOT_EQUALS,
            rewrite=lambda args, rw: rw.replace_node_with(
                rw.call("not").append_arg(rw.call("equal", bool).append_args(args)),
            keep_args=False))

        self.register_rewrite(rewrite.Operator.U_NOT,
            rewrite=lambda args, rw: rw.replace_node_with(rw.call("not")))
        
        self.register_rewrite(rewrite.Operator.IS_SAME,
            rewrite=lambda args, rw: rw.replace_node_with(rw.call("eq")))

        self.register_rewrite(rewrite.Operator.IS_NOT_SAME,
            rewrite=lambda args, rw: rw.replace_node_with(
                rw.call("not").append_arg(rw.call("eq", bool).append_args(args)),
            keep_args=False))

        self.register_rewrite(rewrite.Operator.LESS_THAN,
            rewrite=lambda args, rw: rw.replace_node_with(rw.call("<")))

        self.register_rewrite(rewrite.Operator.GREATER_THAN,
            rewrite=lambda args, rw: rw.replace_node_with(rw.call(">")))

        # str
        self.register_rename(str, arg_type=int, to="int-to-string")

        self.register_rewrite(str.startswith, rename_to="string-prefix-p",
            rewrite=lambda args, rw: rw.rewrite_as_func_call())
        
        self.register_rewrite(str.endswith, rename_to="string-suffix-p",
            rewrite=lambda args, rw: rw.rewrite_as_func_call())

        self.register_rewrite(str.join, rename_to="mapconcat",
            rewrite=lambda args, rw: rw.rewrite_as_func_call()\
                .prepend_arg(rw.xident("identity", nodeattrs.QUOTE_NODE_ATTR)))

        self.register_rewrite(str.strip, rename_to="string-trim",
            rewrite=lambda args, rw: rw.rewrite_as_func_call())

        self.register_rewrite(str.upper, rename_to="upcase",
            rewrite=lambda args, rw: rw.rewrite_as_func_call())

        self.register_rewrite(str.lower, rename_to="downcase",
            rewrite=lambda args, rw: rw.rewrite_as_func_call())

        self.register_rewrite(str.split, rename_to="split-string",
            rewrite=lambda args, rw: rw.rewrite_as_func_call(inst_1st=True))

        self.register_rewrite(str.index, rename_to="cl-search",
            rewrite=lambda args, rw:
                rw.rewrite_as_func_call(inst_1st=False)
                    .update_returned_value(None, -1))

        self.register_rewrite(str.find, rename_to="cl-search",
            rewrite=lambda args, rw:
                rw.rewrite_as_func_call(inst_1st=False)
                    .update_returned_value(None, -1))

        self.register_rename(len, arg_type=str, to="length")

        self.register_rewrite(rewrite.Operator.SUBSCRIPT,
            arg_type=str, rewrite=lambda args, rw: rw.call_with_target_as_arg("substring"))

        # file
        self.register_rewrite(open, arg_type=str,
            rewrite=lambda args, rw: rw.replace_node_with(args[0].node, keep_args=False))

        def _read_rewrite(args, rw, is_readlines):
            rw.rewrite_as_func_call()            
            f = rw.call("with-temp-buffer")\
                .append_to_body(rw.call("insert-file-contents", types.NoneType)
                    .append_args(rw.arg_nodes))\
                .append_to_body(rw.call("buffer-string", str))
            if is_readlines:
                f = rw.call("split-string").append_to_body(f, "\\n")
            rw.replace_node_with(f, keep_args=False)

        self.register_rewrite("read", inst_type=ti.TypeInfo.textiowraper(),
            rewrite=functools.partial(_read_rewrite, is_readlines=False))

        self.register_rewrite("readlines", inst_type=ti.TypeInfo.textiowraper(),
            rewrite=functools.partial(_read_rewrite, is_readlines=True))

        self.register_rewrite("write", inst_type=ti.TypeInfo.textiowraper(),
            rewrite=lambda args, rw:
                rw.rewrite_as_func_call()
                    .replace_node_with(
                        rw.call("with-temp-file")
                            .append_arg(rw.arg_nodes[1])
                            .append_to_body(rw.call("insert").append_arg(rw.arg_nodes[0])),
                        keep_args=False))

        # list
        self.register_rename(len, arg_type=(list, tuple), to="length")

        self.register_rewrite(rewrite.Operator.SUBSCRIPT,
            arg_type=(list, tuple),
            rewrite=lambda args, rw:
                rw.call_with_target_as_arg("nth", target_as_first_arg=False))

        self.register_rewrite(list.sort,
            rewrite=lambda args, rw:
                rw.rewrite_as_func_call().append_arg(rw.less_than(nodeattrs.QUOTE_NODE_ATTR)).reassign_to_arg())

        # dict
        self.register_rewrite(rewrite.Operator.SUBSCRIPT,
            arg_type=dict,
            rewrite=lambda args, rw:
                rw.call_with_target_as_arg("gethash", target_as_first_arg=False))

        self.register_rewrite(rewrite.Operator.DICT_ASSIGNMENT,
            inst_type=dict, rewrite=lambda args, rw:
              rw.call_with_target_as_arg("puthash", target_as_first_arg=False))
        # os
        self.register_attr_rewrite("sep", os, lambda args, rw:
                                   rw.replace_node_with(rw.const("/")))

        # os.path
        self.register_attr_rewrite("sep", os.path, lambda args, rw:
                                   rw.replace_node_with(rw.const("/")))

        # this requires f.el https://github.com/rejeep/f.el
        # (require 'f)
        self.register_rewrite(os.path.join,
            rewrite=lambda args, rw: rw.replace_node_with(rw.call("f-join")))


class ElispFormatter(targetlanguage.AbstractLanguageFormatter):

    def __init__(self):    
        super().__init__(blocks_close_on_same_line=True)

    def requires_space_sep(self, token, remaining_tokens):
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
        return super().requires_space_sep(token, remaining_tokens)

