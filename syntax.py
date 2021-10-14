import ast_token


class Argument:
    """
    Describes a function (== method) argument.
    """

    def __init__(self, node, type):
        """
        node: the argument AST node
        type: the type of the argument
        """
        self.node = node
        self.type = type


class Function:

    def __init__(self, py_name, target_name):
        self.py_name = py_name
        self.target_name = target_name
        self.target_import = None
        self.function_rewrite = None

    def rewrite(self, args, ast_transformer):
        if self.target_name is not None:
            ast_transformer.rename(self.target_name)
        if self.function_rewrite is not None:
            self.function_rewrite(args, ast_transformer)


# arguably this should just be part of the syntax because the formatting
# is not just pretty printing: returntrue for ex
class AbstractLanguageFormatter:
    """
    Formatting customizations.
    """
    pass


class CommonInfixFormatter(AbstractLanguageFormatter):

    def delim_suffix(self, token, remaining_tokens):
        if token.type.is_func_call:
            # no space after func name: print("foo",...
            return False
        if ast_token.is_boundary_ending_before_value_token(
                remaining_tokens, ast_token.FUNC_CALL_BOUNDARY):
            # no space after last func arg: ...,"foo")
            return False
        if ast_token.is_boundary_ending_before_value_token(
                remaining_tokens, ast_token.FUNC_ARG):
            # no space after func arg: 1, 2 - not 1 , 2
            return False
        if token.type.is_func_arg and token.is_end:
            # space after arg sep: 1, 2 - not 1,2
            return True
        if (token.type.is_binop_prec and token.is_end and
            ast_token.next_token_has_value(remaining_tokens)):
            # (1 + 1) * 2, not (1 + 1)* 2
            return True
        if ast_token.is_boundary_ending_before_value_token(
                remaining_tokens, ast_token.BINOP_PREC_BIND):
            # (2 + 3 * 4), not (2 + 3 * 4 )
            return False        
        return token.type.has_value


class PythonFormatter(CommonInfixFormatter):

    def delim_suffix(self, token, remaining_tokens):
        if ast_token.is_boundary_starting_before_value_token(
                remaining_tokens, ast_token.BLOCK):
            # we want if <cond>: (no space between <cond> and :
            return False
        return super().delim_suffix(token, remaining_tokens)
    

class JavaFormatter(CommonInfixFormatter):

    def delim_suffix(self, token, remaining_tokens):
        if ast_token.is_boundary_ending_before_value_token(
                remaining_tokens, ast_token.STMT):
            # we want foo; not foo ;
            return False
        if ast_token.is_boundary_ending_before_value_token(
                remaining_tokens, ast_token.FLOW_CONTROL_TEST):
            # we want if (1 == 1), not if (1 == 1 )
            return False
        if ast_token.is_boundary_starting_before_value_token(
                remaining_tokens, ast_token.BLOCK):
            # we want if (1 == 1) {, not if (1 == 1){
            return True
        if token.type.is_block and token.is_end:
            # we want } else, not }else
            return True
        return super().delim_suffix(token, remaining_tokens)


class ElispFormatter(AbstractLanguageFormatter):

    def delim_suffix(self, token, remaining_tokens):
        if token.type.is_func_call_boundary and token.is_start:
            # no space after '('
            return False
        if ast_token.is_boundary_ending_before_value_token(
                remaining_tokens, ast_token.FUNC_CALL_BOUNDARY):
            # no space if next token is ')'
            return False
        return True



class AbstractLanguageSyntax:
    """
    Stateless metadata that describes a Language Syntax.

    TODO instead of start/end delim, refer to a character pair, like parens,
    curlys etc
    """
    
    def __init__(self, is_prefix,
                 stmt_start_delim, stmt_end_delim,
                 block_start_delim, block_end_delim,
                 flow_control_test_start_delim, flow_control_test_end_delim,
                 arg_delim,
                 strongly_typed):
        self.is_prefix = is_prefix
        self.stmt_start_delim = stmt_start_delim
        self.stmt_end_delim = stmt_end_delim
        self.block_start_delim = block_start_delim
        self.block_end_delim = block_end_delim
        self.flow_control_test_start_delim = flow_control_test_start_delim
        self.flow_control_test_end_delim = flow_control_test_end_delim
        self.arg_delim = arg_delim
        self.strongly_typed = strongly_typed
        self.functions = {}

    def to_literal(self, value):
        if isinstance(value, str):
            return '"%s"' % str(value)
        return value

    def to_identifier(self, value):
        return str(value)

    def register_function_rename(self, py_name, target_name):
        assert not py_name in self.functions
        self.functions[py_name] = Function(py_name, target_name)

    def register_function_rewrite(self, py_name, transform, target_name=None):
        assert not py_name in self.functions
        function = Function(py_name, target_name=target_name)
        function.function_rewrite = transform
        self.functions[py_name] = function
                      

class PythonSyntax(AbstractLanguageSyntax):
    
    def __init__(self):
        """
        : is the block start delim
        """
        super().__init__(is_prefix=False,
                         stmt_start_delim="", stmt_end_delim="",
                         block_start_delim=":", block_end_delim="",
                         flow_control_test_start_delim="", flow_control_test_end_delim="",
                         arg_delim=",",
                         strongly_typed=False,)


class JavaSyntax(AbstractLanguageSyntax):
    
    def __init__(self):
        super().__init__(is_prefix=False,
                         stmt_start_delim="", stmt_end_delim=";",
                         block_start_delim="{", block_end_delim="}",
                         flow_control_test_start_delim="(", flow_control_test_end_delim=")",
                         arg_delim=",",
                         strongly_typed=True,)

        self._fmt = {int: "%d", float: "%d", str: "%s"}

        self.register_function_rewrite(
            py_name="print",
            target_name="System.out.println",
            transform=lambda args, tr:
                tr.replace_args_with(
                    tr.call("String.format")
                        .prepend_arg(" ".join([self._fmt[a.type] for a in args]))
                        .append_args([a.node for a in args]))
                if len(args) > 1 else None)

    def to_literal(self, value):
        if isinstance(value, str):
            return '"%s"' % str(value)
        if isinstance(value, bool):
            return "true" if value else "false"
        return value


class ElispSyntax(AbstractLanguageSyntax):
    
    def __init__(self):
        super().__init__(is_prefix=True,
                         stmt_start_delim="", stmt_end_delim="",
                         block_start_delim="", block_end_delim="",
                         flow_control_test_start_delim="", flow_control_test_end_delim="",
                         arg_delim=" ",
                         strongly_typed=False,)

        self.register_function_rewrite(
            py_name="=",
            transform=lambda args, tr: tr.replace_node_with(tr.call("setq").stmt()))
        self.register_function_rewrite(
            py_name="print",
            target_name="message",
            transform=lambda args, tr:
                tr.prepend_arg(" ".join(["%s" for a in args]))
                if len(args) > 1 or (len(args) == 1 and args[0].type != str) else None)
        self.register_function_rewrite(
            py_name="+",
            transform=lambda args, tr:
                tr.replace_node_with(tr.call("concat")
                    .append_args(
                        [a.node if a.type == str else tr.call("int-to-string")
                            .append_arg(a.node) for a in args]),
                keep_args=False)
                if args[0].type == str else
                # this re-writes the binop node as a call node
                tr.replace_node_with(tr.call("+")))

        self.register_function_rewrite(
            py_name="*",
            transform=lambda args, tr:
                # this re-writes the binop node as a call node
                tr.replace_node_with(tr.call("*")))

        def _if_rewrite(args, tr):
            if_func = tr.call("if").stmt() # stmt -> on its own line
            assert len(tr.node.body) >= 1
            if len(tr.node.body) == 1:
                body = tr.wrap(tr.node.body[0]).newline().indent_incr()
                if len(tr.node.orelse) == 0:
                    body.indent_decr()
                if_func.append_arg(body)
            else:
                # TODO fix formatting - closing parens should not appear on
                # their own lines
                # if the body has more than one stmt, we need to wrap
                # them in a (progn
                progn = tr.call("progn")
                tr.wrap(tr.node.body[0]).newline().indent_incr()
                tr.wrap(tr.node.body[-1])#.indent_decr()
                progn.append_args(tr.node.body)
                if_func.append_arg(progn)
                
            if len(tr.node.orelse) > 0:
                if_func.append_args([tr.wrap(n) for n in tr.node.orelse])
                tr.wrap(tr.node.orelse[-1]).indent_decr()
            tr.replace_node_with(if_func)
        self.register_function_rewrite(py_name="if", transform=_if_rewrite)

        self.register_function_rewrite(
            py_name="==",
            transform=lambda args, tr:
                tr.replace_node_with(tr.call("equal")))
