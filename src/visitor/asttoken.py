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
    def is_class_inst_boundary(self):
        return self is CLASS_INST_BOUNDARY

    @property
    def is_func_def_boundary(self):
        return self in (ANON_FUNC_DEF_BOUNDARY, FUNC_DEF_BOUNDARY)

    @property
    def is_anon(self):
        return self is ANON_FUNC_DEF_BOUNDARY

    @property
    def is_container_literal_boundary(self):
        return self is CONTAINER_LITERAL_BOUNDARY

    @property
    def is_binop(self):
        return self is BINOP

    @property
    def is_binop_prec(self):
        return self is BINOP_PREC_BIND

    @property
    def is_dotop(self):
        return self is DOTOP

    @property
    def is_pointer_deref(self):
        return self is POINTER_DEREF

    @property
    def is_custom_funcdef_end_body_delim(self):
        return self is CUSTOM_FUNCDEF_END_BODY_DELIM

    @property
    def has_value(self):
        return (self.is_literal or
               self.is_identifier or
               self.is_unaryop or
               self.is_import or
               self.is_keyword or
               self.is_pointer_deref or
               self.is_dotop or
               self.is_container_literal_boundary or
               self.is_custom_funcdef_end_body_delim or
               self.is_binop or
               self.is_class_def or
               self.is_func_def or
               self.is_sep)

    @property
    def is_block(self):
        return self in (BLOCK, BLOCK_ON_SAME_LINE)

    @property
    def is_block_on_same_line(self):
        return self is BLOCK_ON_SAME_LINE

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
    def is_class_def(self):
        return self is CLASS_DEF

    @property
    def is_subscript(self):
        return self is SUBSCRIPT

    @property
    def is_type_declaration(self):
        return self is TYPE_DECLARATION

    @property
    def is_type_declaration_rhs(self):
        return self is TYPE_DECLARATION_RHS

    @property
    def is_import(self):
        return self is IMPORT

    @property
    def is_imports(self):
        return self is IMPORTS

    @property
    def is_sep(self):
        return self is SEPARATOR

    def __str__(self):
        return self.name



# tokens that carry a value
UNARYOP = TokenType("UNARYOP")
BINOP = TokenType("BINOP")
DOTOP = TokenType("DOTOP", ".")
IDENTIFIER = TokenType("IDENTIFIER")
LITERAL = TokenType("LITERAL")
FUNC_DEF = TokenType("FUNC_DEF")
CLASS_DEF = TokenType("CLASS_DEF")
IMPORT = TokenType("IMPORT")
KEYWORD = TokenType("KEYWORD") # for/while/if...
KEYWORD_RTN = TokenType("KEYWORD_RTN", "return")
KEYWORD_ELSE = TokenType("KEYWORD_ELSE", "else")
POINTER_DEREF = TokenType("POINTER DEREF", "*")
# multi purpose: stmt sep (for loop), value sep in lists/dicts etc
SEPARATOR = TokenType("SEP")
# optional token provided by function template
CUSTOM_FUNCDEF_END_BODY_DELIM = TokenType("CUSTOM_FUNCDEF_END_BODY_DELIM")


# control tokens
ANON_FUNC_DEF_BOUNDARY = TokenType("ANON_FUNC_DEF_BOUNDARY")
FUNC_DEF_BOUNDARY = TokenType("FUNC_DEF_BOUNDARY")
FUNC_CALL_BOUNDARY = TokenType("FUNC_CALL_BOUNDARY")
# mostly like func_call_boundary, but diff in some languages, like in
# Golang: Foo{} to create a Foo struct, but mostly Foo() (Python, Java ...)
CLASS_INST_BOUNDARY = TokenType("CLASS_INST_BOUNDARY")
FUNC_ARG = TokenType("FUNC_ARG")
BINOP_PREC_BIND = TokenType("BINOP_PREC_BIND")
BLOCK = TokenType("BLOCK")
BLOCK_ON_SAME_LINE = TokenType("BLOCK_SAME_LINE") # unusual, for py lambda
STMT = TokenType("STMT")
BODY_STMT = TokenType("BODY_STMT") # stmt that has a body, like an if stmt
FLOW_CONTROL_TEST = TokenType("FLOW_CONTROL_TEST")
CONTAINER_LITERAL_BOUNDARY = TokenType("CONTAINER_LITERAL_BOUNDARY")
SUBSCRIPT = TokenType("SUBSCRIPT")
TYPE_DECLARATION = TokenType("TYPE_DECLARATION")
TYPE_DECLARATION_RHS = TokenType("TYPE_DECLARATION_RHS")
IMPORTS = TokenType("IMPORTS")

ONE_SPACE = " "


class InProgressFunctionDef:
    def __init__(self):
        self.is_anon = False
        self.scope = None
        self.node_attrs = None
        self.func_name = None
        self.rtn_type_name = None
        self.arg_names = []
        self.arg_types = []


class InProgressTypeDeclaration:
    def __init__(self):
        self.type_names = []
        self.identifiers = []


class TokenConsumer:

    def __init__(self, target):
        self.target = target
        self.indent = 0
        self.current_line = []
        self.lines = [] 
        self.in_progress_function_def = None
        self.in_progress_type_declaration = None
        self.processing_declaration_rhs = False
        self.skip_tokens = False
        self.skip_tokens_boundary_node = None

    def feed(self, token, remaining_tokens):
        postponed_token_handling = False
        if token.type.has_value:
            value = token.value
            if token.type.is_literal:
                value = self.target.to_literal(value)
            elif token.type.is_import:
                if self.target.quote_imports:
                    value = '"%s"' % value
                if not self.target.group_imports:
                    value = "import %s" % value
            elif token.type.is_identifier:
                value = self.target.to_identifier(value)
                if self.in_progress_function_def is not None:
                    self.in_progress_function_def.arg_names.append(value)
                    postponed_token_handling = True
                elif self.in_progress_type_declaration is not None:
                    self.in_progress_type_declaration.identifiers.append(value)
                    postponed_token_handling = True
            elif token.type.is_keyword:
                if self.in_progress_function_def is not None:
                    if token.type.is_rtn:
                        # when defining a function, this is the return type
                        self.in_progress_function_def.rtn_type_name = value
                    else:
                        self.in_progress_function_def.arg_types.append(value)
                    postponed_token_handling = True
                elif self.in_progress_type_declaration is not None:
                    self.in_progress_type_declaration.type_names.append(value)
                    postponed_token_handling = True
                else:
                    if token.type.is_rtn and not self.target.explicit_rtn:
                        postponed_token_handling = True # -> skip it
            elif token.type.is_class_def:
                value = self.target.class_declaration_template.render(token.value)
            elif token.type.is_func_def:
                assert self.in_progress_function_def is not None
                self.in_progress_function_def.func_name = value
                postponed_token_handling = True
            if self.skip_tokens:
                postponed_token_handling = True
            if not postponed_token_handling:
                self._add(value)

        else: # control tokens without values
            if token.type.is_imports:
                if self.target.group_imports:
                    if token.is_start:
                        self._add("import (")
                        self._incr_indent()
                    else:
                        self._decr_indent(remaining_tokens)
                        self._add(")")
                        self._add_newline()
            elif token.type.is_type_declaration_rhs:
                if token.is_start:
                    self.processing_declaration_rhs = True
                else:
                    self.processing_declaration_rhs = False
                    _, _, node = token.value
                    if node is self.skip_tokens_boundary_node:
                        self.skip_tokens = False
            elif token.type.is_type_declaration:
                scope, node_attrs, node = token.value
                if token.is_start:
                    assert self.in_progress_type_declaration is None
                    self.in_progress_type_declaration = InProgressTypeDeclaration()
                else:
                    # this won't quite work for multiple lhs ident/type names
                    # it is close - right now we only support multiple ident
                    # names on the lhs (unpacking)
                    ttdt = self.target.type_declaration_template
                    type_declaration, process_rhs = ttdt.render(
                        ", ".join(self.in_progress_type_declaration.type_names),
                        ", ".join(self.in_progress_type_declaration.identifiers),
                        scope,
                        node_attrs)
                    self._add(type_declaration)
                    self.in_progress_type_declaration = None
                    if not process_rhs:
                        self.skip_tokens = True
                        self.skip_tokens_boundary_node = node

            elif token.type.is_func_arg:
                if self.in_progress_function_def is not None:
                    # function arguments for function signatures are handled
                    # as value tokens above
                    postponed_token_handling = True
                elif self.in_progress_type_declaration is not None and not self.processing_declaration_rhs:
                    # we end up here for the special unpacking case
                    # a, b - we swallow this sep (the comma)
                    pass
                else:
                    if token.is_end:
                        next_token = remaining_tokens[0]
                        boundary_end = next_token.type in (FUNC_CALL_BOUNDARY, CLASS_INST_BOUNDARY) and next_token.is_end
                        if not boundary_end:
                            if self.target.arg_delim == ONE_SPACE:
                                self._add_space()
                            else:
                                self._add(self.target.arg_delim)
            elif token.type.is_binop_prec:
                if token.is_start:
                    self._add_lparen()
                else:
                    self._add_rparen()
            elif token.type.is_stmt:
                if token.is_start:
                    pass
                else:
                    if len(self.current_line) == 0:
                        # edge case when ast nodes are not processed
                        pass
                    else:
                        if not token.type.is_body_stmt:
                            if self.target.stmt_end_delim_always_required:
                                self._add(self.target.stmt_end_delim)
                        if not next_token_has_type(remaining_tokens, BLOCK, is_end=True):
                            self._add_newline()
            elif token.type.is_func_def_boundary:
                if token.is_start:
                    self.in_progress_function_def = InProgressFunctionDef()
                    self.in_progress_function_def.is_anon = token.type.is_anon
                    self.in_progress_function_def.scope = token.value[0]
                    self.in_progress_function_def.node_attrs = token.value[1]
                else:
                    signature = self._build_function_signature()
                    self._add(signature)
                    self.in_progress_function_def = None
            elif token.type.is_func_call_boundary:
                if token.is_start:
                    self._add_lparen()
                if token.is_end:
                    self._add_rparen()
            elif token.type.is_class_inst_boundary:
                if token.is_start:
                    self._add(self.target.object_instantiation_arg_delims[0])
                if token.is_end:
                    self._add(self.target.object_instantiation_arg_delims[1])
            elif token.type.is_subscript:
                if token.is_start:
                    self._add("[")
                else:
                    self._add("]")
            elif token.type.is_flow_control_test:
                if self.target.flow_control_test_delims is not None:
                    if token.is_start:
                        self._add(self.target.flow_control_test_delims[0])
                    else:
                        self._add(self.target.flow_control_test_delims[1])
            elif token.type.is_block:
                if token.is_start:
                    if self.target.block_delims is not None:
                        if len(self.target.block_delims) > 0:
                            self._add(self.target.block_delims[0])
                    if token.type.is_block_on_same_line:
                        self._add_space()
                    else:
                        self._incr_indent()
                else:
                    self._decr_indent(remaining_tokens)
                    if self.target.block_delims is not None:
                        if len(self.target.block_delims) == 2:
                            self._add(self.target.block_delims[1])
        if not postponed_token_handling:
            self._maybe_add_space(token, remaining_tokens)
            self._maybe_add_newline(token, remaining_tokens)

    def __str__(self):
        self._process_current_line()
        return "\n".join(self.lines).strip()

    def _maybe_add_newline(self, token, remaining_tokens):
        if self._requires_newline(token, remaining_tokens):
            self._add_newline()

    def _maybe_add_space(self, token, remaining_tokens):
        handled, add_space = self._requires_space_sep(token, remaining_tokens)
        if handled:
            # first, we checked the hardcode rules in this module to see
            # if a space is needed after the current token
            if add_space:
                self._add_space()
        else:
            # next we check the targetlanguage specific rules
            if self.target.formatter.requires_space_sep(token, remaining_tokens):
                self._add_space()

    def _requires_newline(self, token, remaining_tokens):
        if token.type.is_imports and token.is_end:
            # arriving at LHR in 2h
            return True
        return False

    def _requires_space_sep(self, token, remaining_tokens):
        # fix for java empty ctor but needs to move into java, breaks py
        # however we exit early in this method
        # can we call the target language specific rules first?
        # if token.type.is_func_def_boundary:# and token.is_start:
        #     return True, True
        if is_boundary_ending_before_value_token(remaining_tokens, CONTAINER_LITERAL_BOUNDARY):
            # no space after last container literal arg, for example for list:
            # [..., "foo"] instead of [..., "foo" ]
            return True, False
        if is_boundary_ending_before_value_token(remaining_tokens, STMT):
            # we want foo; not foo ;
            return True, False
        if next_value_token_has_type(remaining_tokens, SEPARATOR):
            # special case for stmts on same line (for loop):
            # we want foo; not foo ;
            # (don't we need STMT_SEPARATOR to be more specific?)
            return True, False
        if next_token_has_type(remaining_tokens, DOTOP):
            # no space before '.': "foo".startswith("f"), not "foo" .startswith
            return True, False
        if token.type.is_binop and next_token_has_type(remaining_tokens, POINTER_DEREF):
            # "hello, " + *self .name, not ... "hello, " +*self .name
            return True, True
        if token.type.is_pointer_deref or next_token_has_type(remaining_tokens, POINTER_DEREF):
            # no space before/after pointer deref: *foo instead of * foo
            # also (*slice)[0] instead of (* slice )[0]
            return True, False
        if token.type.is_dotop:
            # no space after '.': "foo".startswith("f")
            return True, False
        if is_boundary_ending_before_value_token(remaining_tokens, FUNC_CALL_BOUNDARY, CLASS_INST_BOUNDARY):
            # no space after last func arg: ...,"foo")
            return False, False
        if token.type.is_binop and next_token_is(remaining_tokens, "="):
            # a += 1, not a + = 1
            return True, False
        if is_boundary_ending_before_value_token(remaining_tokens, BINOP_PREC_BIND):
            # (2 + 3 * 4), not (2 + 3 * 4 )
            return True, False
        if token.type.is_binop_prec and token.is_end and next_token_has_value(remaining_tokens):
            # (1 + 1) * 2, not (1 + 1)* 2
            return True, True
        if token.type.is_func_arg and token.is_end:
            # space after arg sep: 1, 2 - not 1,2
            return True, True
        if token.type.is_sep and is_within_boundary(remaining_tokens, SUBSCRIPT):
            # "foo"[1:len(blah)], not "foo"[1: len(blah)]
            return True, False
        if token.type.is_subscript and token.is_end:
            # d["k2"] = 3, not d["k2"]= 3
            return True, True
        if next_token_has_type(remaining_tokens, SUBSCRIPT):
            # no space before subscript start: l[0] not l [0]
            # no space before subscript end: l[0] not l[0 ]
            return True, False
        if token.type.is_block and token.is_end:
            # we want } else, not }else
            return True, True
        return False, False

    def _add(self, value):
        value = str(value)
        if len(value) > 0:
            if len(self.current_line) == 0 and self.indent > 0:
                self.current_line.append(" " * self.indent * 4)
            self.current_line.append(value)

    def _add_space(self):
        if len(self.current_line) == 0:
            return
        if self.current_line[-1] == ONE_SPACE:
            return
        if len("".join(self.current_line).strip()) > 0:
            self._add(ONE_SPACE)

    def _add_lparen(self):
        self._add("(")

    def _add_rparen(self):
        self._add(")")

    def _incr_indent(self):
        self.indent += 1
        self._add_newline()

    def _decr_indent(self, remaining_tokens):
        self.indent -= 1
        if not self.target.formatter.blocks_close_on_same_line:
            if len(self.current_line) > 0:
                self._add_newline()

    def _add_newline(self):
        self._process_current_line()

    def _process_current_line(self):
        line = "".join(self.current_line).rstrip()
        self.lines.append(line)
        self.current_line = []

    def _build_function_signature(self):
        arg_names = self.in_progress_function_def.arg_names
        arg_types = self.in_progress_function_def.arg_types
        if self.in_progress_function_def.is_anon:
            template = self.target.anon_function_signature_template
            assert template is not None, "missing anonymous function template"
        else:
            template = self.target.function_signature_template
        signature = template.render(
            self.in_progress_function_def.func_name,
            [(arg_name, arg_types[i] if len(arg_types) > 0 else None) for i, arg_name in enumerate(arg_names)],
            rtn_type=self.in_progress_function_def.rtn_type_name,
            visibility="public",
            scope=self.in_progress_function_def.scope,
            node_attrs=self.in_progress_function_def.node_attrs)
        return signature



def is_boundary_starting_before_value_token(tokens, *token_types):
    """
    Returns True if token.is_start and token.type is the specified token_type
    BEFORE any value token is encountered.
    """
    return _is_boundary_before_value_token(tokens, token_types,
                                           look_for_boundary_start=True)

def is_boundary_ending_before_value_token(tokens, *token_types):
    """
    Returns True if token.is_end and token.type is the specified token_type
    BEFORE any value token is encountered.
    """
    return _is_boundary_before_value_token(tokens, token_types,
                                           look_for_boundary_start=False)


def is_within_boundary(tokens, token_type):
    """
    Returns True if a token with the specified ending (token.is_end) token_type
    is encountered in tokens. Returns False if the token is encountered, but
    it is starting, not ending.
    """
    for token in tokens:
        if token.type is token_type:
            return token.is_end is True
    return False


def _is_boundary_before_value_token(tokens, token_types, look_for_boundary_start):
    for token in tokens:
        if token.type in token_types:
            if look_for_boundary_start:
                if token.is_start:
                    return True
            else:
                if token.is_end:
                    return True
        if token.type.has_value:
            return False
    return False


def next_token_has_value(tokens, requested_value=None):
    if len(tokens) == 0:
        return False
    if tokens[0].type.has_value:
        if requested_value is None:
            return True
        return tokens[0].value == requested_value


def next_token_is(tokens, token_value):
    if len(tokens) == 0:
        return False
    return tokens[0].value == token_value


def next_token_has_type(tokens, token_type, is_end=None):
    if len(tokens) == 0:
        return False
    if tokens[0].type is token_type:
        if is_end is None:
            return True
        else:
            return tokens[0].is_end == is_end


def next_next_token_has_type(tokens, token_type, is_end=None):
    if len(tokens) < 2:
        return False
    if tokens[1].type is token_type:
        if is_end is None:
            return True
        else:
            return tokens[1].is_end == is_end


def next_value_token_has_type(tokens, token_type):
    for t in tokens:
        if t.type.has_value:
            return t.type is token_type
    return False
