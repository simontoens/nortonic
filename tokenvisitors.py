import ast
import ast_token
import visitor


class InfixVisitor(visitor.NoopNodeVisitor):

    def __init__(self, ast_context, language_syntax):
        self.ast_context = ast_context
        self.language_syntax = language_syntax
        self.binop_stack = []
        self.tokens = []

    def expr(self, node, num_children_visited):
        if num_children_visited == -1:
            self.end_statement()

    def block_start(self):
        self.emit_token(ast_token.BLOCK, is_start=True)

    def block_end(self):
        self.emit_token(ast_token.BLOCK, is_start=False)

    def start_statement(self):
        self.emit_token(ast_token.STMT, is_start=True)        
        
    def end_statement(self):
        self.emit_token(type=ast_token.STMT, is_start=False)

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


    # BINOP START
    
    def binop(self, node, num_children_visited):
        binop = _get_binop_for_node(node)        

        if num_children_visited == 0:
            self.binop_start(binop)
            if self.binop_arg_requires_parens():
                self.emit_token(ast_token.BINOP_PREC_BIND, is_start=True)
        elif num_children_visited == -1:
            # visited: left, op, right
            if self.binop_arg_requires_parens():
                self.emit_token(ast_token.BINOP_PREC_BIND, is_start=False)
            self.binop_end(binop)

    def add(self, node, num_children_visited):
        self.emit_token(ast_token.BINOP, "+")

    def mult(self, node, num_children_visited):
        self.emit_token(ast_token.BINOP, "*")

    # BINOP END

    def assign(self, node, num_children_visited):
        if num_children_visited == 0:
            self.start_statement()
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
                self.emit_token(ast_token.KEYWORD, type_name)
                self.emit_token(ast_token.KEYWORD_ARG, is_start=True)
        elif num_children_visited == 1:
            self.emit_token(ast_token.BINOP, "=")
        elif num_children_visited == -1:
            self.emit_token(ast_token.KEYWORD_ARG, is_start=False)
            self.end_statement()

    def call(self, node, num_children_visited):
        if num_children_visited == 0:
            self.emit_token(ast_token.FUNC_CALL_BOUNDARY, is_start=True)
            func_name = _to_target_func_name(node.func.id, self.language_syntax)
            self.emit_token(ast_token.FUNC_CALL, func_name)
        elif num_children_visited == -1:
            self.emit_token(ast_token.FUNC_CALL_BOUNDARY,is_start=False)

    def cond_if(self, node, num_children_visited):
        if num_children_visited == 0:
            self.emit_token(ast_token.KEYWORD, "if")
            self.emit_token(ast_token.KEYWORD_ARG, is_start=True)
            self.emit_token(ast_token.FLOW_CONTROL_TEST, is_start=True)
        elif num_children_visited == 1:
            self.emit_token(ast_token.FLOW_CONTROL_TEST, is_start=False)
            self.emit_token(ast_token.KEYWORD_ARG, is_start=False)
            self.block_start()
        elif num_children_visited == -1:
            self.block_end()

    def cond_else(self, node, num_children_visited):
        if num_children_visited == 0:
            self.emit_token(ast_token.KEYWORD, "else")
            self.block_start()
        elif num_children_visited == -1:
            self.block_end()
            
    def constant(self, node, num_children_visited):
        self.emit_token(ast_token.LITERAL, node.value)

    def eq(self, node, num_children_visited):
        self.emit_token(ast_token.BINOP, "==")

    def name(self, node, num_children_visited):
        self.emit_token(ast_token.IDENTIFIER, node.id)

    def name_constant(self, node, num_children_visited):
        self.emit_token(ast_token.LITERAL, node.value)

    def num(self, node, num_children_visited):
        self.emit_token(ast_token.LITERAL, node.n)

    def rtn(self, node, num_children_visited):
        if num_children_visited == 0:
            self.emit_token(ast_token.KEYWORD, "return")
            self.emit_token(ast_token.KEYWORD_ARG, is_start=True)
        elif num_children_visited == -1:
            self.emit_token(ast_token.KEYWORD_ARG, is_start=False)
            self.end_statement()        

    def string(self, node, num_children_visited):
        self.emit_token(ast_token.LITERAL, node.s)
            
    def emit_token(self, type, value=None, is_start=None):
        self.tokens.append(ast_token.Token(value, type, is_start))


class PrefixVisitor(visitor.NoopNodeVisitor):

    def __init__(self, language_syntax):
        self.language_syntax = language_syntax
        self.tokens = []

    def add(self, node, num_children_visited):
        pass

    def binop(self, node, num_children_visited):
        binop = _get_binop_for_node(node)
        self._emit_func_call(binop.op, num_children_visited)

    def assign(self, node, num_children_visited):
        self._emit_func_call("=", num_children_visited)

    def call(self, node, num_children_visited):
        self._emit_func_call(node.func.id, num_children_visited)

    def compare(self, node, num_children_visited):
        pass

    def cond_if(self, node, num_children_visited):
        pass

    def cond_else(self, node, num_children_visited):
        pass

    def constant(self, node, num_children_visited):
        self.emit_token(ast_token.LITERAL, node.value)

    def eq(self, node, num_children_visited):
        pass

    def expr(self, node, num_children_visited):
        pass
            
    def module(self, node, num_children_visited):
        pass

    def mult(self, node, num_children_visited):
        pass

    def name(self, node, num_children_visited):
        self.emit_token(ast_token.IDENTIFIER, node.id)

    def name_constant(self, node, num_children_visited):
        self.emit_token(ast_token.LITERAL, node.value)

    def num(self, node, num_children_visited):
        self.emit_token(ast_token.LITERAL, node.n)

    def rtn(self, node, num_children_visited):
        pass

    def string(self, node, num_children_visited):
        pass

    def _emit_func_call(self, py_func_name, num_children_visited):
        if num_children_visited == 0:
            func_name = _to_target_func_name(py_func_name, self.language_syntax)
            self.emit_token(ast_token.FUNC_CALL_BOUNDARY, is_start=True)
            self.emit_token(ast_token.FUNC_CALL, func_name)
        elif num_children_visited == -1:
            self.emit_token(ast_token.FUNC_CALL_BOUNDARY, is_start=False)
        
    def emit_token(self, type, value=None, is_start=None):
        self.tokens.append(ast_token.Token(value, type, is_start))



class BinOp:

    def __init__(self, op, precedence):
        self.op = op
        self.precedence = precedence

    def __str__(self):
        return self.op


ADD_BINOP = BinOp("+", 1)
MULT_BINOP = BinOp("*", 2)


def _get_binop_for_node(node):
    if isinstance(node.op, ast.Add):
        return ADD_BINOP
    elif isinstance(node.op, ast.Mult):
        return MULT_BINOP
    else:
        assert False, "bad binop node %s" % node
    

def _to_target_func_name(py_func_name, syntax):
    func = syntax.functions.get(py_func_name, None)
    return py_func_name if func is None else func.target_name



