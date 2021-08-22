class Function:
    
    def __init__(self, py_name, py_import, target_name, target_import):
        self.py_name = py_name
        self.py_import = py_import
        self.target_name = target_name
        self.target_import = target_import


class AbstractLanguageSyntax:
    """
    Stateless metadata that describes a Language Syntax.
    """
    
    def __init__(self, is_prefix, statement_delim,
                 block_start_delim, block_end_delim,
                 block_cond_start_delim, block_cond_end_delim,
                 strongly_typed, tokens_requiring_sep):
        self.is_prefix = is_prefix
        self.statement_delim = statement_delim
        self.block_start_delim = block_start_delim
        self.block_end_delim = block_end_delim
        self.block_cond_start_delim = block_cond_start_delim
        self.block_cond_end_delim = block_cond_end_delim
        self.tokens_requiring_sep = tokens_requiring_sep
        self.strongly_typed = strongly_typed
        self.functions = {}

    def to_literal(self, value):
        if isinstance(value, str):
            return '"%s"' % str(value)
        return value

    def to_identifier(self, value):
        return str(value)

    def token_requires_sep(self, token):
        return token in self.tokens_requiring_sep

    def register_function(self, function):
        self.functions[function.py_name] = function
                      

class PythonSyntax(AbstractLanguageSyntax):
    
    def __init__(self):
        super().__init__(is_prefix=False,
                         statement_delim="",
                         block_start_delim=":", block_end_delim="",
                         block_cond_start_delim="", block_cond_end_delim="",
                         strongly_typed=False,
                         tokens_requiring_sep=("if", "return",))


class JavaSyntax(AbstractLanguageSyntax):
    
    def __init__(self):
        super().__init__(is_prefix=False,
                         statement_delim=";",
                         block_start_delim="{", block_end_delim="}",
                         block_cond_start_delim="(", block_cond_end_delim=")",
                         strongly_typed=True,
                         tokens_requiring_sep=("int", "return",))
        self.register_function(Function("print", None,
                                        "System.out.println", None))
