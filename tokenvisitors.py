import ast
import ast_token
import nodeattrs
import visitor


class TokenVisitor(visitor.NoopNodeVisitor):

    def __init__(self, ast_context, language_syntax):
        self.ast_context = ast_context
        self.language_syntax = language_syntax
        self.binop_stack = []

        self.tokens = []

        self.is_visiting_attr = False

    def block_start(self):
        self.emit_token(ast_token.BLOCK, is_start=True)

    def block_end(self):
        self.emit_token(ast_token.BLOCK, is_start=False)

    def attr(self, node, num_children_visited):
        if num_children_visited == 0:
            self.is_visiting_attr = True
        elif num_children_visited == -1:
            self.is_visiting_attr = False
            t = ast_token.IDENTIFIER
            # refactor - this almost the same logic as in name(...)
            if len(self.tokens) > 1 and self.tokens[-2].type.is_func_call_boundary and self.tokens[-2].is_start:
                t = ast_token.FUNC_CALL
            self.emit_token(ast_token.TARGET_DEREF)
            self.emit_token(t, node.attr)

    def call(self, node, num_children_visited):
        if num_children_visited == 0:
            self._handle_formatting_directives(node, num_children_visited)
            self.emit_token(ast_token.FUNC_CALL_BOUNDARY, is_start=True)
        elif num_children_visited > 1:
            self.emit_token(ast_token.FUNC_ARG, is_start=False)
        if num_children_visited == -1:
            self.emit_token(ast_token.FUNC_CALL_BOUNDARY, is_start=False)
            self._handle_formatting_directives(node, num_children_visited)

    def constant(self, node, num_children_visited):
        self.emit_token(ast_token.LITERAL, node.value)

    def expr(self, node, num_children_visited):
        if num_children_visited == 0:
            self._handle_formatting_directives(node, num_children_visited)
        if num_children_visited == -1:
            if not hasattr(node, nodeattrs.BLOCK_START_NODE_ATTR):
                self.end_statement()
            self._handle_formatting_directives(node, num_children_visited)

    def name(self, node, num_children_visited):
        t = ast_token.IDENTIFIER
        if len(self.tokens) > 0 and self.tokens[-1].type.is_func_call_boundary and self.tokens[-1].is_start:
            if self.is_visiting_attr:
                # this is the 'a' of a.startswith, nothing to do
                pass
            else:
                # this is the function name
                t = ast_token.FUNC_CALL
        self.emit_token(t, node.id)

    def name_constant(self, node, num_children_visited):
        self.emit_token(ast_token.LITERAL, node.value)

    def num(self, node, num_children_visited):
        self.emit_token(ast_token.LITERAL, node.n)

    def lst(self, node, num_children_visited):
        if num_children_visited == 0:
            self.emit_token(ast_token.LIST_LITERAL_BOUNDARY, is_start=True)
        elif num_children_visited > 0:
            # list literal arguments look like function arguments
            self.emit_token(ast_token.FUNC_ARG, is_start=False)
        elif num_children_visited == -1:
            self.emit_token(ast_token.LIST_LITERAL_BOUNDARY, is_start=False)

    def string(self, node, num_children_visited):
        self.emit_token(ast_token.LITERAL, node.s)

    def emit_token(self, type, value=None, is_start=None):
        self.tokens.append(ast_token.Token(value, type, is_start))
            
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
                lhs = node.targets[0]
                lhs_type_info = self.ast_context.lookup_type_info_by_node(lhs)
                assert lhs_type_info is not None, "lhs type info is None"
                rhs = node.value
                rhs_type_info = self.ast_context.lookup_type_info_by_node(rhs)
                assert rhs_type_info is not None, "rhs type info is None"
                assert lhs_type_info == rhs_type_info, "type insanity"
                t = rhs_type_info.value_type
                assert t in self.language_syntax.type_mappings, "No type mapping for %s" % t
                type_mapping = self.language_syntax.type_mappings[t]
                type_name = type_mapping.target_name
                self.emit_token(ast_token.KEYWORD, type_name)
                self.emit_token(ast_token.KEYWORD_ARG, is_start=True)
        elif num_children_visited == 1:
            self.emit_token(ast_token.BINOP, "=")
        elif num_children_visited == -1:
            self.emit_token(ast_token.KEYWORD_ARG, is_start=False)
            self.end_statement()

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
            self.emit_token(ast_token.KEYWORD_ELSE)
            self.block_start()
        elif num_children_visited == -1:
            self.block_end()
            
    def eq(self, node, num_children_visited):
        self.emit_token(ast_token.BINOP, "==")

    def funcdef(self, node, num_children_visited):
        pass

    def rtn(self, node, num_children_visited):
        if num_children_visited == 0:
            self._handle_formatting_directives(node, num_children_visited)
            self.emit_token(ast_token.KEYWORD_RTN)
            self.emit_token(ast_token.KEYWORD_ARG, is_start=True)
        elif num_children_visited == -1:
            self.emit_token(ast_token.KEYWORD_ARG, is_start=False)
            self._handle_formatting_directives(node, num_children_visited)
            self.end_statement()

    def _handle_formatting_directives(self, node, num_children_visited):
        if num_children_visited == 0:
            if hasattr(node, nodeattrs.BLOCK_START_NODE_ATTR):
                self.block_start()
            if hasattr(node, nodeattrs.NEWLINE_NODE_ATTR):
                self.emit_token(ast_token.NEWLINE, is_start=True)
            if hasattr(node, nodeattrs.INDENT_INCR_NODE_ATTR):
                self.emit_token(ast_token.INDENT, is_start=True)
            if hasattr(node, nodeattrs.INDENT_AROUND_NODE_ATTR):
                self.emit_token(ast_token.INDENT, is_start=True)
        elif num_children_visited == -1:
            if hasattr(node, nodeattrs.BLOCK_START_NODE_ATTR):
                self.block_end()
            if hasattr(node, nodeattrs.STMT_NODE_ATTR):
                self.end_statement()
            if hasattr(node, nodeattrs.INDENT_DECR_NODE_ATTR):
                self.emit_token(ast_token.INDENT, is_start=False)
            if hasattr(node, nodeattrs.INDENT_AROUND_NODE_ATTR):
                self.emit_token(ast_token.INDENT, is_start=False)

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
