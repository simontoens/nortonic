class Token:
    
    def __init__(self, value, type, is_start=None):
        assert type is not None
        assert value is None or is_start is None
        self.value = value
        self.type = type
        # some tokens have a natural start/end meaning, for ex:
        # start block, end block - this boolean represents this concept
        self._is_start = is_start        

    @property
    def is_start(self):
        assert self._is_start is not None
        return self._is_start

    @property
    def is_end(self):
        return not self.is_start

    def __str__(self):
        return str(self.type) + ("" if self.value is None else " " + str(self.value)) + ("" if self._is_start is None else " start: " + str(self._is_start))

class TokenType:
    
    def __init__(self, name, value=None):
        self.name = name

    @property
    def is_literal(self):
        return self is LITERAL

    @property
    def is_identifier(self):
        return self is IDENTIFIER

    @property
    def is_func_call(self):
        return self is FUNC_CALL

    @property
    def is_func_call_boundary(self):
        return self is FUNC_CALL_BOUNDARY

    @property
    def is_binop_prec(self):
        return self is BINOP_PREC_BIND

    @property
    def has_value(self):
        return (self.is_literal or
                self.is_identifier or
                self in (BINOP, KEYWORD, FUNC_CALL))

    @property
    def is_block(self):
        return self is BLOCK

    @property
    def is_flow_control_test(self):
        return self is FLOW_CONTROL_TEST

    @property
    def is_keyword(self):
        return self is KEYWORD

    @property
    def is_keyword_arg(self):
        return self is KEYWORD_ARG

    @property
    def is_stmt(self):
        return self is STMT

    @property
    def is_func_arg(self):
        return self is FUNC_ARG

    def __str__(self):
        return self.name


# value
BINOP = TokenType("BINOP")
IDENTIFIER = TokenType("IDENTIFIER")
LITERAL = TokenType("LITERAL")
FUNC_CALL = TokenType("FUNC_CALL") # rename to FUNC_NAME?
KEYWORD = TokenType("KEYWORD") # return/for/while/if...

# control
FUNC_CALL_BOUNDARY = TokenType("FUNC_CALL_BOUNDARY")
FUNC_ARG = TokenType("FUNC_ARG")
BINOP_PREC_BIND = TokenType("BINOP_PREC_BIND")
BLOCK = TokenType("BLOCK")
STMT = TokenType("STMT")
FLOW_CONTROL_TEST = TokenType("FLOW_CONTROL_TEST") # rename to CONDITIONAL?
KEYWORD_ARG = TokenType("KEYWORD_ARG")


class TokenConsumer:

    def __init__(self, syntax, formatter):
        self.current_line = []
        self.lines = []
        self.syntax = syntax
        self.formatter = formatter
        self.indentation = 0

    def feed(self, token, remaining_tokens):
        if token.type.has_value:
            value = token.value
            if token.type.is_literal:
                value = self.syntax.to_literal(value)
            elif token.type.is_identifier:
                value = self.syntax.to_identifier(value)
            if token.type.is_func_call and self.syntax.is_prefix:
                self._add_lparen()
            self._add(value)
            if token.type.is_func_call and not self.syntax.is_prefix:
                self._add_lparen()
        else:
            if token.type.is_func_arg:
                if token.is_end:
                    if not remaining_tokens[0].type.is_func_call_boundary:
                        self._add(self.syntax.arg_delim)
            elif token.type.is_binop_prec:
                if token.is_start:
                    self._add_lparen()
                else:
                    self._add_rparen()
            elif token.type.is_stmt:
                if token.is_start:
                    self._add(self.syntax.stmt_start_delim)
                else:
                    self._add(self.syntax.stmt_end_delim)
                    self._add_newline()
            elif token.type.is_func_call_boundary:
                if not token.is_start:
                    self._add_rparen()
            elif token.type.is_flow_control_test:
                if token.is_start:
                    self._add(self.syntax.flow_control_test_start_delim)
                else:
                    self._add(self.syntax.flow_control_test_end_delim)
            elif token.type.is_block:
                if token.is_start:
                    self._add(self.syntax.block_start_delim)
                    self._add_newline()
                    self._incr_indent()
                else:
                    self._decr_indent()
                    self._add(self.syntax.block_end_delim)
                    if len(remaining_tokens) > 0:
                        # use token_type instead of checking for token value
                        next_else = remaining_tokens[0].value == "else"
                    else:
                        next_else = False
                    if not next_else and len(self.syntax.block_end_delim) > 0:
                        self._add_newline()
        if self.formatter.delim_suffix(token, remaining_tokens):
            self._add_delim()

    def __str__(self):
        self._process_current_line()
        return "\n".join(self.lines).strip()

    def _add(self, value):
        self.current_line.append(str(value))

    def _add_delim(self):
        delim = " " # space is the standard delimiter ...
        if len(self.current_line) == 0 or self.current_line[-1] != delim:
            self._add(delim)
        
    def _add_lparen(self):
        self._add("(")

    def _add_rparen(self):
        self._add(")")

    def _incr_indent(self):
        self.indentation += 1

    def _decr_indent(self):
        self.indentation -= 1

    def _add_newline(self):
        self._process_current_line()

    def _get_indentation_str(self):
        indent_num_spaces = 4
        return self.indentation * indent_num_spaces * " "

    def _process_current_line(self):
        if len(self.current_line) > 0:
            line = "".join(self.current_line).strip()
            self.lines.append("%s%s" % (self._get_indentation_str(), line))
            self.current_line = []

    def _is_else(self, token):
        # or make it a specific token type?
        return token is not None and token.is_keyword and token.value == "else"


def is_boundary_starting_before_value_token(tokens, token_type):
    """
    Returns True if token.is_start and token.type is the specified token_type
    BEFORE any value token is encountered.
    """
    return _is_boundary_before_value_token(tokens, token_type,
                                           look_for_boundary_start=True)

def is_boundary_ending_before_value_token(tokens, token_type):
    """
    Returns True if token.is_end and token.type is the specified token_type
    BEFORE any value token is encountered.
    """
    return _is_boundary_before_value_token(tokens, token_type,
                                           look_for_boundary_start=False)


def _is_boundary_before_value_token(tokens, token_type, look_for_boundary_start):
    for token in tokens:
        if token.type.has_value:
            return False
        else:
            if look_for_boundary_start:
                if token.is_start and token.type is token_type:
                    return True
            else:
                if token.is_end and token.type is token_type:
                    return True
    return False


def next_token_has_value(tokens):
    if len(tokens) == 0:
        return False
    return tokens[0].type.has_value
