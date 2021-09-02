import ast_token


class Function:
    
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

class PythonFormatter(AbstractLanguageFormatter):

    def delim_prefix(self, token):
        return token.type in (ast_token.BINOP,
                              ast_token.KEYWORD_ARG)

    def delim_suffix(self, token):
        return token.type in (ast_token.BINOP,)
    

class JavaFormatter(AbstractLanguageFormatter):

    def delim_prefix(self, token):
        return token.type in (ast_token.BLOCK,
                              ast_token.BINOP,
                              ast_token.KEYWORD_ARG)

    def delim_suffix(self, token):
        return token.type in (ast_token.BLOCK,
                              ast_token.BINOP)
    

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
                 strongly_typed,
                 token_types_requiring_delim_suffix):
        self.is_prefix = is_prefix
        self.stmt_start_delim = stmt_start_delim
        self.stmt_end_delim = stmt_end_delim
        self.block_start_delim = block_start_delim
        self.block_end_delim = block_end_delim
        self.flow_control_test_start_delim = flow_control_test_start_delim
        self.flow_control_test_end_delim = flow_control_test_end_delim
        self.token_types_requiring_delim_suffix = token_types_requiring_delim_suffix,
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
        super().__init__(is_prefix=False,
                         stmt_start_delim="", stmt_end_delim="",
                         block_start_delim=":", block_end_delim="",
                         flow_control_test_start_delim="", flow_control_test_end_delim="",
                         strongly_typed=False,
                         token_types_requiring_delim_suffix=(),)


class JavaSyntax(AbstractLanguageSyntax):
    
    def __init__(self):
        super().__init__(is_prefix=False,
                         stmt_start_delim="", stmt_end_delim=";",
                         block_start_delim="{", block_end_delim="}",
                         flow_control_test_start_delim="(", flow_control_test_end_delim=")",
                         strongly_typed=True,
                         token_types_requiring_delim_suffix=(),)
        self.register_function(Function("print", "System.out.println"))

        
class ElispSyntax(AbstractLanguageSyntax):
    
    def __init__(self):
        super().__init__(is_prefix=True,
                         stmt_start_delim="(", stmt_end_delim=")",
                         block_start_delim="", block_end_delim="",
                         flow_control_test_start_delim="", flow_control_test_end_delim="",
                         strongly_typed=False,
                         token_types_requiring_delim_suffix=(),)
        self.register_function(Function("print", "message"))
        
