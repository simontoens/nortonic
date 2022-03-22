class Token:

    def __init__(self, value, type, is_start=None):
        assert type is not None
        assert value is None or is_start is None
        self._value = value
        self.type = type
        # some tokens have a natural start/end meaning, for ex:
        # start block, end block - this boolean represents this concept
        self._is_start = is_start

    @property
    def value(self):
        if self._value is None:
            return self.type.value
        else:
            return self._value

    @property
    def is_start(self):
        assert self._is_start is not None
        return self._is_start

    @property
    def is_end(self):
        return not self.is_start

    def __str__(self):
        s = str(self.type)
        if self.value is not None:
            s += " " + str(self.value)
        if self._is_start is not None:
            s += " start" if self._is_start else " end"
        return s

    __repr__ = __str__


class TokenType:

    def __init__(self, name, value=None):
        self.name = name
        self.value = value # some tokens have a fixed value based on type

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
    def is_func_def_boundary(self):
        return self is FUNC_DEF_BOUNDARY

    @property
    def is_list_literal_boundary(self):
        return self is LIST_LITERAL_BOUNDARY

    @property
    def is_binop_prec(self):
        return self is BINOP_PREC_BIND

    @property
    def is_target_deref(self):
        return self is TARGET_DEREF

    @property
    def has_value(self):
        return (self.is_literal or
                self.is_identifier or
                self.is_keyword or
                self.is_target_deref or
                self in (BINOP, FUNC_CALL, FUNC_DEF,))

    @property
    def is_block(self):
        return self is BLOCK

    @property
    def is_flow_control_test(self):
        return self is FLOW_CONTROL_TEST

    @property
    def is_keyword(self):
        return self in (KEYWORD, KEYWORD_ELSE, KEYWORD_RTN)

    @property
    def is_rtn(self):
        return self is KEYWORD_RTN

    @property
    def is_else(self):
        return self is KEYWORD_ELSE

    @property
    def is_keyword_arg(self):
        return self is KEYWORD_ARG

    @property
    def is_stmt(self):
        return self is STMT

    @property
    def is_func_arg(self):
        return self is FUNC_ARG

    @property
    def is_func_def(self):
        return self is FUNC_DEF

    @property
    def is_indent_control(self):
        return self is INDENT

    @property
    def is_newline(self):
        return self is NEWLINE

    def __str__(self):
        return self.name


# value
BINOP = TokenType("BINOP")
IDENTIFIER = TokenType("IDENTIFIER")
LITERAL = TokenType("LITERAL")
FUNC_CALL = TokenType("FUNC_CALL")
FUNC_DEF = TokenType("FUNC_DEF")
KEYWORD = TokenType("KEYWORD") # for/while/if...
KEYWORD_RTN = TokenType("KEYWORD_RTN", "return")
KEYWORD_ELSE = TokenType("KEYWORD_ELSE", "else")
TARGET_DEREF = TokenType("DEREF", ".")

# control
FUNC_DEF_BOUNDARY = TokenType("FUNC_DEF_BOUNDARY")
FUNC_CALL_BOUNDARY = TokenType("FUNC_CALL_BOUNDARY")
FUNC_ARG = TokenType("FUNC_ARG")
BINOP_PREC_BIND = TokenType("BINOP_PREC_BIND")
BLOCK = TokenType("BLOCK")
STMT = TokenType("STMT")
FLOW_CONTROL_TEST = TokenType("FLOW_CONTROL_TEST")
KEYWORD_ARG = TokenType("KEYWORD_ARG")
INDENT = TokenType("INDENT")
NEWLINE = TokenType("NEWLINE")
LIST_LITERAL_BOUNDARY = TokenType("LIST_LITERAL_BOUNDARY")

DEFAULT_DELIM = " "


class InProgressFunctionDef:
    def __init__(self):
        self.func_name = None
        self.arg_names = []
        self.arg_types = []


class TokenConsumer:

    def __init__(self, syntax, formatter):
        self.syntax = syntax
        self.formatter = formatter
        self.indent = 0
        self.current_line = []
        self.lines = [] 
        self.in_progress_function_def = None

    def feed(self, token, remaining_tokens):
        #print(token)
        if not self.syntax.explicit_rtn and token.type.is_rtn:
            return
        if token.type.has_value:
            value = token.value
            assert value is not None
            if token.type.is_literal:
                value = self.syntax.to_literal(value)
            elif token.type.is_identifier:
                value = self.syntax.to_identifier(value)
                if self.in_progress_function_def is not None:
                    self.in_progress_function_def.arg_names.append(value)
                    value = None # > dev/null
            elif token.type.is_keyword:
                if self.in_progress_function_def is not None:
                    self.in_progress_function_def.arg_types.append(value)
                    value = None # > dev/null                
            elif token.type.is_func_def:
                assert self.in_progress_function_def is not None
                self.in_progress_function_def.func_name = value
                value = None # > /dev/null
            if token.type.is_func_call and self.syntax.is_prefix:
                self._add_lparen()
            if value is not None:
                self._add(value)
            if token.type.is_func_call and not self.syntax.is_prefix:
                self._add_lparen()
        else:
            if token.type.is_func_arg:
                if self.in_progress_function_def is None:
                    if token.is_end:
                        next_token = remaining_tokens[0]
                        boundary_end = next_token.type in (FUNC_CALL_BOUNDARY, LIST_LITERAL_BOUNDARY) and next_token.is_end
                        if not boundary_end:
                            if self.syntax.arg_delim == DEFAULT_DELIM:
                                self._add_delim()
                            else:
                                self._add(self.syntax.arg_delim)
                else:
                    # function arguments for function signatures are handled
                    # as value tokens above
                    pass
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
                    if is_boundary_ending_before_value_token(remaining_tokens, FUNC_CALL_BOUNDARY):
                        # although the condition is written generically, this
                        # is only for elisp (at this point), where we end up
                        # with stmts nested in func calls
                        # for example, for the input
                        # if 1==2:
                        #     print("foo")
                        # we want:
                        # (if (eq 1 2)
                        #     (message "foo")) <--
                        # not:
                        # (if (eq 1 2)
                        #     (message "foo")
                        # ) <--
                        pass
                    else:
                        self._add_newline()
            elif token.type.is_func_def_boundary:
                if token.is_start:
                    self.in_progress_function_def = InProgressFunctionDef()
                else:
                    arg_names = self.in_progress_function_def.arg_names
                    arg_types = self.in_progress_function_def.arg_types
                    signature = self.syntax.function_signature_template.render(
                        self.in_progress_function_def.func_name,
                        [(arg_name, arg_types[i] if len(arg_types) > 0 else None) for i, arg_name in enumerate(arg_names)])
                    self._add(signature)
                    self.in_progress_function_def = None
            elif token.type.is_func_call_boundary:
                if token.is_end:
                    self._add_rparen()
            elif token.type.is_list_literal_boundary:
                if token.is_start:
                    self._add("[")
                else:
                    self._add("]")
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
                    next_token_is_else = False
                    if len(remaining_tokens) > 0:
                        next_token_is_else = remaining_tokens[0].type.is_else
                    if not next_token_is_else and len(self.syntax.block_end_delim) > 0:
                        self._add_newline()
            elif token.type.is_indent_control:
                if token.is_start:
                    self._incr_indent()
                else:
                    self._decr_indent()
            elif token.type.is_newline:
                self._add_newline()
        if self.formatter.delim_suffix(token, remaining_tokens):
            self._add_delim()

    def __str__(self):
        self._process_current_line()
        return "\n".join(self.lines).strip()

    def _add(self, value):
        value = str(value)
        if len(value) > 0:
            if len(self.current_line) == 0 and self.indent > 0:
                self.current_line.append(" " * self.indent * 4)
            self.current_line.append(value)

    def _add_delim(self):
        if len(self.current_line) == 0:
            return
        if self.current_line[-1] == DEFAULT_DELIM:
            return
        if len("".join(self.current_line).strip()) > 0:
             self._add(DEFAULT_DELIM)

    def _add_lparen(self):
        self._add("(")

    def _add_rparen(self):
        self._add(")")

    def _incr_indent(self):
        self.indent += 1

    def _decr_indent(self):
        assert self.indent > 0
        self.indent -= 1

    def _add_newline(self):
        if len(self.current_line) > 0:
            self._process_current_line()

    def _process_current_line(self):
        if len(self.current_line) > 0:
            line = "".join(self.current_line).rstrip()
            self.lines.append(line)
            self.current_line = []


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

def next_token_has_type(tokens, token_type):
    if len(tokens) == 0:
        return False
    return tokens[0].type is token_type
