import ast
import token
import visitor


class BinOp:

    def __init__(self, op, precedence):
        self.op = op
        self.precedence = precedence

    def __str__(self):
        return self.op


ADD_BINOP = BinOp("+", 1)
MULT_BINOP = BinOp("*", 2)


class LanguageEmitterVisitor(visitor.NoopNodeVisitor):
    """
    TODO: introduce abstraction between this class and the writing of text
    this class should create tokens and emit them, that way formatting can
    be handled by different logic that processes the token stream
    this other logic can also figure out after which tokens to add spaces
    """

    def __init__(self, ast_context, language_syntax, token_consumer):
        self.ast_context = ast_context
        self.language_syntax = language_syntax
        self.tokens = []
        self.indentation_level = 0
        self.binop_stack = []
        self.token_consumer = token_consumer

    def expr(self, node, num_children_visited):
        if num_children_visited == -1:
            self.end_statement()

    def block_start(self):
        self.indentation_level += 1
        self.append(self.language_syntax.block_start_delim)
        self.append("\n")

    def block_end(self):
        self.indentation_level -= 1
        self.append(self.language_syntax.block_end_delim)        

    def start_statement(self):
        self.append(self.language_syntax.stmt_start_delim)
        
    def end_statement(self):
        self.append(self.language_syntax.stmt_end_delim)
        self.append("\n")

    def binop_start(self, binop):
        self.binop_stack.append(binop)

    def binop_end(self, binop):
        self.binop_stack.pop()

    def binop_arg_requires_parens(self):
        if len(self.binop_stack) > 1:
            current_op = self.binop_stack[-1]
            parent_op = self.binop_stack[-2]
            if current_op.precedence < parent_op.precedence:
                return True
        return False

    def get_indentation(self):
        return "    "*self.indentation_level
    
    def append(self, token):
        if len(self.tokens) > 0 and self.tokens[-1] == "\n":
            self.tokens.append(self.get_indentation())
        self.tokens.append(str(token))
        if self.language_syntax.token_requires_sep(token):
            self.tokens.append(" ")

    def __str__(self):
        return "".join(self.tokens).strip()


    # BINOP START
    
    def binop(self, node, num_children_visited):
        if isinstance(node.op, ast.Add):
            binop = ADD_BINOP
        elif isinstance(node.op, ast.Mult):
            binop = MULT_BINOP
        else:
            assert False, "bad binop"

        if num_children_visited == 0:
            self.binop_start(binop)
            if self.binop_arg_requires_parens():
                self.append("(")
        elif num_children_visited == -1:
            # visited: left, op, right
            if self.binop_arg_requires_parens():
                self.append(")")
            self.binop_end(binop)

    def add(self, node, num_children_visited):
        self.append("+")

    def mult(self, node, num_children_visited):
        self.append("*")

    # BINOP END

    def assign(self, node, num_children_visited):
        if num_children_visited == 0:
            self.start_statement()
            self.emit_token(type=token.STMT_START)
            if self.language_syntax.is_prefix:
                self.append("=")
                self.emit_token(token.BINOP, "=")
            if self.language_syntax.strongly_typed:
                rhs = node.value
                rhs_type_info = self.ast_context.lookup_type_info_by_node(rhs)
                type_name = "<no type information>"
                if rhs_type_info is not None:
                    t = rhs_type_info.value_type
                    # FIXME - add real type mapper, owned by lang syntax
                    if t is int:
                        type_name = "int"
                    elif t is float:
                        type_name = "float"
                    elif t is str:
                        type_name = "String"
                    else:
                        type_name = "<unknown type>"
                self.append(type_name)
                self.emit_token(token.TYPE_DECL, type_name)
        elif num_children_visited == 1:
            if not self.language_syntax.is_prefix:
                self.append("=")
                self.emit_token(token.BINOP, "=")
        elif num_children_visited == -1:
            self.end_statement()
            self.emit_token(type=token.STMT_END)

    def call(self, node, num_children_visited):
        if num_children_visited == 0:
            py_func_name = node.func.id
            func = self.language_syntax.functions.get(py_func_name, None)
            func_name = py_func_name if func is None else func.target_name
            self.append(func_name)
            self.append("(")
        elif num_children_visited == -1:
            self.append(")")

    def cond_if(self, node, num_children_visited):
        if num_children_visited == 0:
            self.append("if")
            self.append(self.language_syntax.block_cond_start_delim)
        elif num_children_visited == 1:
            self.append(self.language_syntax.block_cond_end_delim)
            self.block_start()
        elif num_children_visited == -1:
            self.block_end()

    def cond_else(self, node, num_children_visited):
        if num_children_visited == 0:
            self.append("else")
            self.block_start()
        elif num_children_visited == -1:
            self.block_end()
            
    def constant(self, node, num_children_visited):
        self.append(self.language_syntax.to_literal(node.value))

    def eq(self, node, num_children_visited):
        self.append("==")

    def name(self, node, num_children_visited):
        self.emit_token(token.IDENTIFIER, node.id)
        self.append(self.language_syntax.to_identifier(node.id))

    def name_constant(self, node, num_children_visited):
        self.append(self.language_syntax.to_identifier(node.value))        

    def num(self, node, num_children_visited):
        self.emit_token(token.NUMBER, node.n)
        self.append(self.language_syntax.to_literal(node.n))

    def rtn(self, node, num_children_visited):
        if num_children_visited == 0:
            self.append("return")
        elif num_children_visited == -1:
            self.end_statement()        

    def string(self, node, num_children_visited):
        self.emit_token(token.STRING, node.s)
        self.append(self.language_syntax.to_literal(node.s))
            
    def emit_token(self, type, value=None):
        self.token_consumer.feed(token.Token(value, type))
