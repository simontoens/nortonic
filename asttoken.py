import scopes


class Token:

    def __init__(self, value, type, is_start=None):
        assert type is not None
        self._value = value
        self.type = type
        # some tokens have a natural beginning and end, for ex:
        # start block, end block - this boolean represents the start/end concept
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
    def is_unaryop(self):
        return self is UNARYOP

    @property
    def is_func_call_boundary(self):
        return self is FUNC_CALL_BOUNDARY

    @property
    def is_func_def_boundary(self):
        return self is FUNC_DEF_BOUNDARY

    @property
    def is_container_literal_boundary(self):
        return self is CONTAINER_LITERAL_BOUNDARY

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
                self.is_unaryop or
                self.is_keyword or
                self.is_target_deref or
                self.is_container_literal_boundary or
                self in (BINOP, FUNC_DEF, VALUE_SEPARATOR))

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
    def is_value_sep(self):
        return self is VALUE_SEPARATOR

    @property
    def is_rtn(self):
        return self is KEYWORD_RTN

    @property
    def is_else(self):
        return self is KEYWORD_ELSE

    @property
    def is_stmt(self):
        return self in (STMT, BODY_STMT)

    @property
    def is_body_stmt(self):
        return self is BODY_STMT

    @property
    def is_func_arg(self):
        return self is FUNC_ARG

    @property
    def is_func_def(self):
        return self is FUNC_DEF

    @property
    def is_subscript(self):
        return self is SUBSCRIPT

    @property
    def is_type_declaration(self):
        return self is TYPE_DECLARATION

    def __str__(self):
        return self.name


# value
UNARYOP = TokenType("UNARYOP")
BINOP = TokenType("BINOP")
IDENTIFIER = TokenType("IDENTIFIER")
LITERAL = TokenType("LITERAL")
FUNC_DEF = TokenType("FUNC_DEF")
KEYWORD = TokenType("KEYWORD") # for/while/if...
KEYWORD_RTN = TokenType("KEYWORD_RTN", "return")
KEYWORD_ELSE = TokenType("KEYWORD_ELSE", "else")
TARGET_DEREF = TokenType("DEREF", ".")
VALUE_SEPARATOR = TokenType("VALUE_SEP")

# control
FUNC_DEF_BOUNDARY = TokenType("FUNC_DEF_BOUNDARY")
FUNC_CALL_BOUNDARY = TokenType("FUNC_CALL_BOUNDARY")
FUNC_ARG = TokenType("FUNC_ARG")
BINOP_PREC_BIND = TokenType("BINOP_PREC_BIND")
BLOCK = TokenType("BLOCK")
STMT = TokenType("STMT")
BODY_STMT = TokenType("BODY_STMT") # stmt that has a body, like an if stmt
FLOW_CONTROL_TEST = TokenType("FLOW_CONTROL_TEST")
CONTAINER_LITERAL_BOUNDARY = TokenType("CONTAINER_LITERAL_BOUNDARY")
SUBSCRIPT = TokenType("SUBSCRIPT")
TYPE_DECLARATION = TokenType("TYPE_DECLARATION")

DEFAULT_DELIM = " "


class InProgressFunctionDef:
    def __init__(self):
        self.func_name = None
        self.rtn_type_name = None
        self.arg_names = []
        self.arg_types = []


class InProgressTypeDeclaration:
    def __init__(self):
        self.scope = None
        self.type_name = None
        self.identifier = None
        

class TokenConsumer:

    def __init__(self, target):
        self.target = target
        self.indent = 0
        self.current_line = []
        self.lines = [] 
        self.in_progress_function_def = None
        self.in_progress_type_declaration = None

    def feed(self, token, remaining_tokens):
        if token.type.has_value:
            value = token.value
            if token.type.is_literal:
                value = self.target.to_literal(value)
            elif token.type.is_identifier:
                value = self.target.to_identifier(value)
                if self.in_progress_function_def is not None:
                    self.in_progress_function_def.arg_names.append(value)
                    value = None # > dev/null
                elif self.in_progress_type_declaration is not None:
                    self.in_progress_type_declaration.identifier = value
                    value = None # > dev/null
            elif token.type.is_keyword:
                if self.in_progress_function_def is not None:
                    if token.type.is_rtn:
                        # when defining a function, this is the return type
                        self.in_progress_function_def.rtn_type_name = value
                    else:
                        self.in_progress_function_def.arg_types.append(value)
                    value = None # > dev/null
                elif self.in_progress_type_declaration is not None:
                    self.in_progress_type_declaration.type_name = value
                    value = None # > dev/null
                else:
                    if token.type.is_rtn and not self.target.explicit_rtn:
                        value = None # > dev/null
            elif token.type.is_func_def:
                assert self.in_progress_function_def is not None
                self.in_progress_function_def.func_name = value
                value = None # > /dev/null
            if value is not None:
                self._add(value)
        else:
            if token.type.is_type_declaration:
                if token.is_start:
                    assert self.in_progress_type_declaration is None
                    self.in_progress_type_declaration = InProgressTypeDeclaration()
                    self.in_progress_type_declaration.scope = token.value
                else:
                    type_declaration = self.target.type_declaration_template.\
                        render(self.in_progress_type_declaration.type_name,
                               self.in_progress_type_declaration.identifier,
                               self.in_progress_type_declaration.scope)
                    self._add(type_declaration)
                    self.in_progress_type_declaration = None
            elif token.type.is_func_arg:
                if self.in_progress_function_def is None:
                    if token.is_end:
                        next_token = remaining_tokens[0]
                        boundary_end = next_token.type in (FUNC_CALL_BOUNDARY,) and next_token.is_end
                        if not boundary_end:
                            if self.target.arg_delim == DEFAULT_DELIM:
                                self._add_delim()
                            else:
                                self._add(self.target.arg_delim)
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
                    self._add(self.target.stmt_start_delim)
                else:
                    if len(self.current_line) == 0:
                        # edge case when ast nodes are not processed
                        pass
                    else:
                        if not token.type.is_body_stmt:
                            self._add(self.target.stmt_end_delim)
            elif token.type.is_func_def_boundary:
                if token.is_start:
                    self.in_progress_function_def = InProgressFunctionDef()
                else:
                    arg_names = self.in_progress_function_def.arg_names
                    arg_types = self.in_progress_function_def.arg_types
                    signature = self.target.function_signature_template.render(
                        self.in_progress_function_def.func_name,
                        [(arg_name, arg_types[i] if len(arg_types) > 0 else None) for i, arg_name in enumerate(arg_names)],
                        rtn_type=self.in_progress_function_def.rtn_type_name)
                    self._add(signature)
                    self.in_progress_function_def = None
            elif token.type.is_func_call_boundary:
                if token.is_start:
                    self._add_lparen()
                if token.is_end:
                    self._add_rparen()
            elif token.type.is_subscript:
                if token.is_start:
                    self._add("[")
                else:
                    self._add("]")
            elif token.type.is_flow_control_test:
                if token.is_start:
                    self._add(self.target.flow_control_test_start_delim)
                else:
                    self._add(self.target.flow_control_test_end_delim)
            elif token.type.is_block:
                if token.is_start:
                    self._add(self.target.block_start_delim)
                    self._add_newline()
                    self._incr_indent()
                else:
                    self._decr_indent()
                    self._add(self.target.block_end_delim)
        if self.target.formatter.delim_suffix(token, remaining_tokens):
            self._add_delim()
        if self.target.formatter.newline(token, remaining_tokens):
            self._add_newline()

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
        if token.type is token_type:
            if look_for_boundary_start:
                if token.is_start:
                    return True
            else:
                if token.is_end:
                    return True
        if token.type.has_value:
            return False
    return False


def next_token_has_value(tokens):
    if len(tokens) == 0:
        return False
    return tokens[0].type.has_value


def next_token_has_type(tokens, token_type):
    if len(tokens) == 0:
        return False
    return tokens[0].type is token_type


def next_next_token_has_type(tokens, token_type, is_end=None):
    if len(tokens) < 2:
        return False
    if tokens[1].type is token_type:
        if is_end is None:
            return True
        else:
            return tokens[1].is_end == is_end
