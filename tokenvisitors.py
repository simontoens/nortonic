import ast
import asttoken
import nodeattrs
import null
import visitor


class TokenVisitor(visitor.NoopNodeVisitor):

    def __init__(self, ast_context, language_syntax):
        super().__init__()        
        self.ast_context = ast_context
        self.language_syntax = language_syntax
        self.binop_stack = []

        self.tokens = []

        self.is_visiting_attr = False

        # hack to handle no-args (== no children)
        self._funcdef_args_next = False

    def block_start(self):
        self.emit_token(asttoken.BLOCK, is_start=True)

    def block_end(self):
        self.emit_token(asttoken.BLOCK, is_start=False)

    def attr(self, node, num_children_visited):
        if num_children_visited == 0:
            self.is_visiting_attr = True
        elif num_children_visited == -1:
            self.is_visiting_attr = False
            t = asttoken.IDENTIFIER
            # refactor - this almost the same logic as in name(...)
            if len(self.tokens) > 1 and self.tokens[-2].type.is_func_call_boundary and self.tokens[-2].is_start:
                t = asttoken.FUNC_CALL
            self.emit_token(asttoken.TARGET_DEREF)
            self.emit_token(t, node.attr)

    def call(self, node, num_children_visited):
        if num_children_visited == 0:
            self._handle_formatting_directives(node, num_children_visited)
            self.emit_token(asttoken.FUNC_CALL_BOUNDARY, is_start=True)
        elif num_children_visited > 1:
            self.emit_token(asttoken.FUNC_ARG, is_start=False)
        if num_children_visited == -1:
            self.emit_token(asttoken.FUNC_CALL_BOUNDARY, is_start=False)
            self._handle_formatting_directives(node, num_children_visited)

    def constant(self, node, num_children_visited):
        value = node.value
        if value is None:
            value = null.value
        self.emit_token(asttoken.LITERAL, value)

    def expr(self, node, num_children_visited):
        if num_children_visited == 0:
            self._handle_formatting_directives(node, num_children_visited)
        if num_children_visited == -1:
            if not hasattr(node, nodeattrs.BLOCK_START_NODE_ATTR):
                self.end_statement()
            self._handle_formatting_directives(node, num_children_visited)

    def funcarg(self, node, num_children_visited):
        type_info = self.ast_context.lookup_type_info_by_node(node)
        self.emit_token(asttoken.FUNC_ARG, is_start=True)
        if self.language_syntax.strongly_typed:
            arg_type_info = self.ast_context.lookup_type_info_by_node(node)
            arg_type_name = self.language_syntax.type_mapper.lookup_target_type_name(arg_type_info)            
            self.emit_token(asttoken.KEYWORD, arg_type_name)
        self.emit_token(asttoken.IDENTIFIER, node.arg)
        self.emit_token(asttoken.FUNC_ARG, is_start=False)

    def funcdef(self, node, num_children_visited):
        if num_children_visited == 0 and not self._funcdef_args_next:
            self._funcdef_args_next = True
            self.emit_token(asttoken.FUNC_DEF_BOUNDARY, is_start=True)
            self.emit_token(asttoken.FUNC_DEF, node.name)
        elif self._funcdef_args_next:
            self._funcdef_args_next = False
            self.emit_token(asttoken.FUNC_DEF_BOUNDARY, is_start=False)
            self.block_start()
        elif num_children_visited == -1:
            self.block_end()

    def name(self, node, num_children_visited):
        t = asttoken.IDENTIFIER
        if len(self.tokens) > 0 and self.tokens[-1].type.is_func_call_boundary and self.tokens[-1].is_start:
            if self.is_visiting_attr:
                # this is the 'a' of a.startswith, nothing to do
                pass
            else:
                # this is the function name
                t = asttoken.FUNC_CALL
        self.emit_token(t, node.id)

    def num(self, node, num_children_visited):
        self.emit_token(asttoken.LITERAL, node.n)

    def lst(self, node, num_children_visited):
        if num_children_visited == 0:
            self.emit_token(asttoken.LIST_LITERAL_BOUNDARY, is_start=True)
        elif num_children_visited > 0:
            # list literal arguments look like function arguments
            self.emit_token(asttoken.FUNC_ARG, is_start=False)
        elif num_children_visited == -1:
            self.emit_token(asttoken.LIST_LITERAL_BOUNDARY, is_start=False)

    def string(self, node, num_children_visited):
        self.emit_token(asttoken.LITERAL, node.s)

    def emit_token(self, type, value=None, is_start=None):
        self.tokens.append(asttoken.Token(value, type, is_start))

    def start_statement(self):
        self.emit_token(asttoken.STMT, is_start=True)

    def end_statement(self):
        self.emit_token(type=asttoken.STMT, is_start=False)

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


    def binop(self, node, num_children_visited):
        binop = _get_binop_for_node(node)

        if num_children_visited == 0:
            self.binop_start(binop)
            if self.binop_arg_requires_parens():
                self.emit_token(asttoken.BINOP_PREC_BIND, is_start=True)
        elif num_children_visited == -1:
            # visited: left, op, right
            if self.binop_arg_requires_parens():
                self.emit_token(asttoken.BINOP_PREC_BIND, is_start=False)
            self.binop_end(binop)

    def add(self, node, num_children_visited):
        self.emit_token(asttoken.BINOP, "+")

    def mult(self, node, num_children_visited):
        self.emit_token(asttoken.BINOP, "*")

    def assign(self, node, num_children_visited):
        if num_children_visited == 0:
            self.start_statement()
            if self.language_syntax.strongly_typed:
                lhs = node.targets[0]
                scope = self.ast_context.current_scope.get()
                if scope.is_declaration_node(lhs):
                    lhs_type_info = self.ast_context.lookup_type_info_by_node(lhs)
                    assert lhs_type_info is not None, "lhs type info is None"
                    rhs = node.value
                    rhs_type_info = self.ast_context.lookup_type_info_by_node(rhs)
                    assert rhs_type_info is not None, "rhs type info is None"
                    assert lhs_type_info == rhs_type_info, "type insanity"
                    target_type_name = self.language_syntax.type_mapper.lookup_target_type_name(lhs_type_info)
                    if target_type_name is None:
                        # this happens if the rhs of the assignment is None
                        # for example
                        # a=None
                        # check wether a is ever given another value
                        # TODO check for mixed type assignemnts (and fail)?
                        for other_lhs in scope.get_ident_nodes_by_name(lhs.id):
                            lhs_type_info = self.ast_context.lookup_type_info_by_node(other_lhs)
                            target_type_name = self.language_syntax.type_mapper.lookup_target_type_name(lhs_type_info)
                            if target_type_name is not None:
                                break
                        else:
                            raise Exception("Unable to determine type of ident [%s]" % lhs.id)
                        
                    self.emit_token(asttoken.KEYWORD, target_type_name)
                    self.emit_token(asttoken.KEYWORD_ARG, is_start=True)
        elif num_children_visited == 1:
            self.emit_token(asttoken.BINOP, "=")
        elif num_children_visited == -1:
            self.emit_token(asttoken.KEYWORD_ARG, is_start=False)
            self.end_statement()

    def cond_if(self, node, num_children_visited):
        if num_children_visited == 0:
            self.emit_token(asttoken.KEYWORD, "if")
            self.emit_token(asttoken.KEYWORD_ARG, is_start=True)
            self.emit_token(asttoken.FLOW_CONTROL_TEST, is_start=True)
        elif num_children_visited == 1:
            self.emit_token(asttoken.FLOW_CONTROL_TEST, is_start=False)
            self.emit_token(asttoken.KEYWORD_ARG, is_start=False)
            self.block_start()
        elif num_children_visited == -1:
            self.block_end()

    def cond_else(self, node, num_children_visited):
        if num_children_visited == 0:
            self.emit_token(asttoken.KEYWORD_ELSE)
            self.block_start()
        elif num_children_visited == -1:
            self.block_end()

    def eq(self, node, num_children_visited):
        self.emit_token(asttoken.BINOP, "==")

    def rtn(self, node, num_children_visited):
        if num_children_visited == 0:
            self._handle_formatting_directives(node, num_children_visited)
            self.emit_token(asttoken.KEYWORD_RTN)
            self.emit_token(asttoken.KEYWORD_ARG, is_start=True)
        elif num_children_visited == -1:
            self.emit_token(asttoken.KEYWORD_ARG, is_start=False)
            self._handle_formatting_directives(node, num_children_visited)
            self.end_statement()

    def _handle_formatting_directives(self, node, num_children_visited):
        if num_children_visited == 0:
            if hasattr(node, nodeattrs.BLOCK_START_NODE_ATTR):
                self.block_start()
            if hasattr(node, nodeattrs.NEWLINE_NODE_ATTR):
                self.emit_token(asttoken.NEWLINE, is_start=True)
            if hasattr(node, nodeattrs.INDENT_INCR_NODE_ATTR):
                self.emit_token(asttoken.INDENT, is_start=True)
            if hasattr(node, nodeattrs.INDENT_AROUND_NODE_ATTR):
                self.emit_token(asttoken.INDENT, is_start=True)
        elif num_children_visited == -1:
            if hasattr(node, nodeattrs.BLOCK_START_NODE_ATTR):
                self.block_end()
            if hasattr(node, nodeattrs.STMT_NODE_ATTR):
                self.end_statement()
            if hasattr(node, nodeattrs.INDENT_DECR_NODE_ATTR):
                self.emit_token(asttoken.INDENT, is_start=False)
            if hasattr(node, nodeattrs.INDENT_AROUND_NODE_ATTR):
                self.emit_token(asttoken.INDENT, is_start=False)

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
