class Token:
    
    def __init__(self, value, type):
        self.value = value
        self.type = type


class TokenType:
    
    def __init__(self, name):
        self.name = name

    @property
    def is_literal(self):
        return self in (NUMBER, STRING)

    @property
    def is_identifier(self):
        return self == IDENTIFIER

    @property
    def has_value(self):
        return self.is_literal or self.is_identifier or self in (BINOP, TYPE_DECL)

    @property
    def is_stmt_end(self):
        return self == STMT_END


BINOP = TokenType("BINOP")
IDENTIFIER = TokenType("IDENTIFIER")
NUMBER = TokenType("NUMBER")
STRING = TokenType("STRING")
TYPE_DECL = TokenType("TYPE_DECL")

BLOCK_START = TokenType("BLOCK_START")
BLOCK_END = TokenType("BLOCK_END")
STMT_START = TokenType("STMT_START")
STMT_END = TokenType("STMT_END")


class Formatter:

    def __init__(self, syntax):
        self.current_line = []
        self.lines = []
        self.syntax = syntax

    def feed(self, token):
        if token.type.has_value:
            value = token.value
            if token.type.is_literal:
                value = self.syntax.to_literal(value)
            elif token.type.is_identifier:
                value = self.syntax.to_identifier(value)
            self._add(value)
        else:
            if token.type.is_stmt_end:
                self._add(self.syntax.stmt_end_delim, prefix_with_space=False)

    def __str__(self):
        # should be called instead by explicit "done" method?
        self._process_current_line()
        return "\n".join(self.lines).strip()

    def _process_current_line(self):
        if len(self.current_line) > 0:
            self.lines.append("".join(self.current_line).strip())
            self.current_line = []

    def _add(self, value, prefix_with_space=True):
        if prefix_with_space:
            self.current_line.append(" ")
        self.current_line.append(str(value))
