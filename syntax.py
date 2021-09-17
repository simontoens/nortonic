import ast_token


class Function:
    # TODO a function needs to also describe its argument, spefically its types
    # for example, in python/java: print(1) works, but in elisp (message 1) does not
    
    def __init__(self, py_name, target_name, target_import=None):
        self.py_name = py_name
        self.target_name = target_name
        self.target_import = target_import


# arguably this should just be part of the syntax because the formatting
# is not just pretty printing: returntrue for ex
class AbstractLanguageFormatter:
    """
    Formatting customizations.
    """
    pass


class CommonInfixFormatter(AbstractLanguageFormatter):

    def delim_suffix(self, token, remaining_tokens):
        if token.type is ast_token.FUNC_CALL:
            # no space after func name: print("foo",...
            return False
        if ast_token.is_boundary_ending_before_value_token(
                remaining_tokens, ast_token.FUNC_CALL_BOUNDARY):
            # no space after las func arg: ...,"foo")
            return False
        if (token.type is ast_token.BINOP_PREC_BIND and
            token.is_end and
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
        if token.type is ast_token.BLOCK and token.is_end:
            # we want } else, not }else
            return True
        return super().delim_suffix(token, remaining_tokens)


class ElispFormatter(AbstractLanguageFormatter):

    def delim_suffix(self, token, remaining_tokens):
        if token.type is ast_token.FUNC_CALL_BOUNDARY and token.is_start:
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
                 strongly_typed):
        self.is_prefix = is_prefix
        self.stmt_start_delim = stmt_start_delim
        self.stmt_end_delim = stmt_end_delim
        self.block_start_delim = block_start_delim
        self.block_end_delim = block_end_delim
        self.flow_control_test_start_delim = flow_control_test_start_delim
        self.flow_control_test_end_delim = flow_control_test_end_delim
        self.strongly_typed = strongly_typed
        self.functions = {}

    def to_literal(self, value):
        if isinstance(value, str):
            return '"%s"' % str(value)
        return value

    def to_identifier(self, value):
        return str(value)

    def register_function(self, function):
        self.functions[function.py_name] = function
                      

class PythonSyntax(AbstractLanguageSyntax):
    
    def __init__(self):
        """
        : is the block start delim
        """
        super().__init__(is_prefix=False,
                         stmt_start_delim="", stmt_end_delim="",
                         block_start_delim=":", block_end_delim="",
                         flow_control_test_start_delim="", flow_control_test_end_delim="",
                         strongly_typed=False,)


class JavaSyntax(AbstractLanguageSyntax):
    
    def __init__(self):
        super().__init__(is_prefix=False,
                         stmt_start_delim="", stmt_end_delim=";",
                         block_start_delim="{", block_end_delim="}",
                         flow_control_test_start_delim="(", flow_control_test_end_delim=")",
                         strongly_typed=True,)
        self.register_function(Function("print", "System.out.println"))


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
                         strongly_typed=False,)
        self.register_function(Function("=", "setq"))
        self.register_function(Function("print", "message"))
        
