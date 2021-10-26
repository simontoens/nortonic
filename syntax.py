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


class TypeMapping:

    def __init__(self, py_type, target_name, literal_converter):
        self.py_type = py_type
        self.target_name = target_name
        self.literal_converter = literal_converter


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
                 strongly_typed,
                 explicit_rtn=True):
        self.is_prefix = is_prefix
        self.stmt_start_delim = stmt_start_delim
        self.stmt_end_delim = stmt_end_delim
        self.block_start_delim = block_start_delim
        self.block_end_delim = block_end_delim
        self.flow_control_test_start_delim = flow_control_test_start_delim
        self.flow_control_test_end_delim = flow_control_test_end_delim
        self.arg_delim = arg_delim
        self.strongly_typed = strongly_typed
        self.explicit_rtn = explicit_rtn

        self.functions = {}
        self.type_mappings = {}

    def to_literal(self, value):
        if isinstance(value, str):
            return '"%s"' % str(value)
        value_type = type(value)
        if value_type in self.type_mappings:
            type_mapping = self.type_mappings[value_type]
            if type_mapping.literal_converter is not None:
                return type_mapping.literal_converter(value)
        return value

    def to_identifier(self, value):
        return str(value)

    def combine_types(self, lhs, rhs):
        # this is incomplete at best
        # if specialized behavior is necessary, it can move into the
        # TypeMapping class?
        if lhs is float or rhs is float:
            return float
        if lhs is str or rhs is str:
            return str
        return int

    def register_function_rename(self, py_name, target_name):
        assert not py_name in self.functions
        self.functions[py_name] = Function(py_name, target_name)

    def register_function_rewrite(self, py_name, transform, target_name=None):
        assert not py_name in self.functions
        function = Function(py_name, target_name=target_name)
        function.function_rewrite = transform
        self.functions[py_name] = function

    def register_type_mapping(self, py_type, target_name, literal_converter=None):
        assert py_type not in self.functions
        type_mapping = TypeMapping(py_type, target_name, literal_converter)
        self.type_mappings[py_type] = type_mapping


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

        self.register_type_mapping(int,  "int")
        self.register_type_mapping(float,  "float")
        self.register_type_mapping(str,  "String")
        self.register_type_mapping(bool, "boolean", lambda v: "true" if v else "false")

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


class ElispSyntax(AbstractLanguageSyntax):
    
    def __init__(self):
        super().__init__(is_prefix=True,
                         stmt_start_delim="", stmt_end_delim="",
                         block_start_delim="", block_end_delim="",
                         flow_control_test_start_delim="", flow_control_test_end_delim="",
                         arg_delim=" ",
                         strongly_typed=False,
                         explicit_rtn=False)

        self.register_type_mapping(bool, None, lambda v: "t" if v else "nil")

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
            if_func = tr.call("if").stmt() # stmt so (if ..) is followed by \n
            assert len(tr.node.body) >= 1
            if_block_has_single_stmt = len(tr.node.body) == 1
            else_block_exists = len(tr.node.orelse) > 0
            if if_block_has_single_stmt:
                body = tr.wrap(tr.node.body[0]).newline().indent_incr()
                if not else_block_exists:
                    body.indent_decr()
                if_func.append_arg(body)
            else:
                progn = tr.call("progn").newline().indent_around()
                tr.wrap(tr.node.body[0]).newline().indent_incr()
                tr.wrap(tr.node.body[-1]).newline().indent_decr()
                progn.append_args(tr.node.body)
                if_func.append_arg(progn)
            if else_block_exists:
                if_func.append_args([tr.wrap(n) for n in tr.node.orelse])
                if not if_block_has_single_stmt:
                    tr.wrap(tr.node.orelse[0]).newline().indent_incr()
                tr.wrap(tr.node.orelse[-1]).newline().indent_decr()
            tr.replace_node_with(if_func)
        self.register_function_rewrite(py_name="if", transform=_if_rewrite)

        self.register_function_rewrite(
            py_name="==",
            transform=lambda args, tr:
                tr.replace_node_with(tr.call("equal")))
