import ast
import asttoken
import nodeattrs
import visitor


START_MARK = "START_MARK"
END_MARK = "END_MARK"


class TokenVisitor(visitor.NoopNodeVisitor):

    def __init__(self, ast_context, syntax):
        super().__init__()        
        self.ast_context = ast_context
        self.syntax = syntax
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
            self.emit_token(asttoken.TARGET_DEREF)
            self.emit_token(asttoken.IDENTIFIER, node.attr)

    def call(self, node, num_children_visited):
        if num_children_visited == 0:
            self._handle_formatting_directives(node, num_children_visited)
            if self.syntax.is_prefix:
                self.emit_token(asttoken.FUNC_CALL_BOUNDARY, is_start=True)
        elif num_children_visited == 1:
            if not self.syntax.is_prefix:
                self.emit_token(asttoken.FUNC_CALL_BOUNDARY, is_start=True)
        elif num_children_visited > 1:
            self.emit_token(asttoken.FUNC_ARG, is_start=False)
        if num_children_visited == -1:
            self.emit_token(asttoken.FUNC_CALL_BOUNDARY, is_start=False)
            self._handle_formatting_directives(node, num_children_visited)

    def constant(self, node, num_children_visited):
        self.emit_token(asttoken.LITERAL, node.value)

    def expr(self, node, num_children_visited):
        if num_children_visited == 0:
            self._handle_formatting_directives(node, num_children_visited)
        if num_children_visited == -1:
            if not hasattr(node, nodeattrs.BLOCK_START_NODE_ATTR):
                self.end_statement()
            self._handle_formatting_directives(node, num_children_visited)

    def loop_for(self, node, num_children_visited):
        super().loop_for(node, num_children_visited)
        if num_children_visited == 0:
            self.emit_token(asttoken.KEYWORD, "for")
            self.emit_token(asttoken.FLOW_CONTROL_TEST, is_start=True)
            if self.syntax.strongly_typed:
                type_info = self.ast_context.lookup_type_info_by_node(node.target)
                target_type_name = self.syntax.type_mapper.lookup_target_type_name(type_info)
                self.emit_token(asttoken.KEYWORD, target_type_name)
        elif num_children_visited == 1:
            self.emit_token(asttoken.KEYWORD, self.syntax.loop_foreach_keyword)
        elif num_children_visited == 2:
            self.emit_token(asttoken.FLOW_CONTROL_TEST, is_start=False)
            self.block_start()
        elif num_children_visited == -1:
            self.block_end()

    def funcarg(self, node, num_children_visited):
        type_info = self.ast_context.lookup_type_info_by_node(node)
        self.emit_token(asttoken.FUNC_ARG, is_start=True)
        if self.syntax.strongly_typed:
            arg_type_info = self.ast_context.lookup_type_info_by_node(node)
            arg_type_name = self.syntax.type_mapper.lookup_target_type_name(arg_type_info)            
            self.emit_token(asttoken.KEYWORD, arg_type_name)
        self.emit_token(asttoken.IDENTIFIER, node.arg)
        self.emit_token(asttoken.FUNC_ARG, is_start=False)

    def funcdef(self, node, num_children_visited):
        if num_children_visited == 0 and not self._funcdef_args_next:
            self._funcdef_args_next = True
            self.emit_token(asttoken.FUNC_DEF_BOUNDARY, is_start=True)
            self.emit_token(asttoken.FUNC_DEF, node.name)
            if self.syntax.strongly_typed:
                func = self.ast_context.get_function(node.name)
                rtn_type_info = func.get_rtn_type_info()
                if rtn_type_info.value_type == None.__class__:
                    # method does not return anything, ie void
                    pass
                else:
                    rtn_type_name = self.syntax.type_mapper.lookup_target_type_name(rtn_type_info)
                    # hacky (?) way to pass through the return type
                    self.emit_token(asttoken.KEYWORD_RTN, rtn_type_name)
        elif self._funcdef_args_next:
            self._funcdef_args_next = False
            self.emit_token(asttoken.FUNC_DEF_BOUNDARY, is_start=False)
            self.block_start()
        elif num_children_visited == -1:
            self.block_end()

    def name(self, node, num_children_visited):
        self.emit_token(asttoken.IDENTIFIER, node.id)

    def num(self, node, num_children_visited):
        self.emit_token(asttoken.LITERAL, node.n)

    def container_type_dict(self, node, num_children_visited):
        super().container_type_dict(node, num_children_visited)
        type_info = self.ast_context.lookup_type_info_by_node(node)
        type_mapping = self.syntax.type_mapper.get_type_mapping(type_info)
        if num_children_visited == 0:
            self.emit_token(asttoken.CONTAINER_LITERAL_BOUNDARY,
                            value=type_mapping.start_literal,
                            is_start=True)
        elif num_children_visited == -1:
            self.emit_token(asttoken.CONTAINER_LITERAL_BOUNDARY,
                            value=type_mapping.end_literal,
                            is_start=False)
        elif num_children_visited % 2 == 0:
            if num_children_visited / 2 < len(node.keys):
                self.emit_token(asttoken.FUNC_ARG, is_start=False)
        else: # num_children_visited > 0:
            self.emit_token(asttoken.VALUE_SEPARATOR,
                            value=type_mapping.value_separator)

    def container_type_list(self, node, num_children_visited):
        super().container_type_list(node, num_children_visited)
        self._container_type_sequence(node, num_children_visited, list)

    def container_type_tuple(self, node, num_children_visited):
        super().container_type_list(node, num_children_visited)
        self._container_type_sequence(node, num_children_visited, tuple)

    def _container_type_sequence(self, node, num_children_visited, py_type):
        type_info = self.ast_context.lookup_type_info_by_node(node)
        type_mapping = self.syntax.type_mapper.get_type_mapping(type_info)
        if num_children_visited == 0:
            self.emit_token(asttoken.CONTAINER_LITERAL_BOUNDARY,
                            value=type_mapping.start_literal,
                            is_start=True)
        elif num_children_visited == -1:
            self.emit_token(asttoken.CONTAINER_LITERAL_BOUNDARY,
                            value=type_mapping.end_literal,
                            is_start=False)
        else:  # num_children_visited > 0:
            if num_children_visited < len(node.elts):
                # list literal arguments look like function arguments
                self.emit_token(asttoken.FUNC_ARG, is_start=False)

    def string(self, node, num_children_visited):
        self.emit_token(asttoken.LITERAL, node.s)

    def emit_token(self, type, value=None, is_start=None):
        self.tokens.append(asttoken.Token(value, type, is_start))

    def emit_tokens(self, tokens):
        self.tokens.extend(tokens)

    def start_token_mark(self):
        self.tokens.append(START_MARK)

    def end_token_mark(self):
        self.tokens.append(END_MARK)

    def cut_marked_tokens(self):
        end_mark_index = -1
        for i in range(len(self.tokens) - 1, 0, -1):
            if end_mark_index == -1:
                if self.tokens[i] == END_MARK:
                    end_mark_index = i
            else:
                if self.tokens[i] == START_MARK:
                    cut_tokens = self.tokens[i+1:end_mark_index]
                    self.tokens = self.tokens[0:i] + self.tokens[end_mark_index+1:]
                    return cut_tokens
        raise Exception("nothing to cut")

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

    def unaryop(self, node, num_children_visited):
        if num_children_visited == 0:
            assert isinstance(node.op, ast.USub), node.op
            self.emit_token(asttoken.UNARYOP, "-")

    def add(self, node, num_children_visited):
        self.emit_token(asttoken.BINOP, "+")

    def div(self, node, num_children_visited):
        self.emit_token(asttoken.BINOP, "/")

    def mult(self, node, num_children_visited):
        self.emit_token(asttoken.BINOP, "*")

    def assign(self, node, num_children_visited):
        if num_children_visited == 0:
            self.start_statement()
            if self.syntax.strongly_typed:
                lhs = node.targets[0]
                scope = self.ast_context.current_scope.get()
                if scope.is_declaration_node(lhs):
                    lhs_type_info = self.ast_context.lookup_type_info_by_node(lhs)
                    assert lhs_type_info is not None, "lhs type info is None"
                    rhs = node.value
                    rhs_type_info = self.ast_context.lookup_type_info_by_node(rhs)
                    assert rhs_type_info is not None, "rhs type info is None"
                    assert lhs_type_info == rhs_type_info, "type insanity"
                    target_type_name = self.syntax.type_mapper.lookup_target_type_name(lhs_type_info)
                    if target_type_name is None:
                        # this happens if the rhs of the assignment is None
                        # for example
                        # a=None
                        # check wether a is ever given another value
                        # TODO check for mixed type assignemnts (and fail)?
                        for other_lhs in scope.get_ident_nodes_by_name(lhs.id):
                            lhs_type_info = self.ast_context.lookup_type_info_by_node(other_lhs)
                            target_type_name = self.syntax.type_mapper.lookup_target_type_name(lhs_type_info)
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

    def cond_if(self, node, num_children_visited, is_expr):
        if is_expr:
            if self.syntax.ternary_replaces_if_expr:
                if num_children_visited == 0:
                    # capture "body" and replay it after the conditional test
                    # has been emitted
                    # another (more elegant?) approach would be to support
                    # a way to specify a custom ast traversal order
                    # test -> if-branch -> else vs python's if-branch test else
                    self.start_token_mark()
                elif num_children_visited == 1:
                    self.end_token_mark()
                elif num_children_visited == -1:
                    self.emit_token(asttoken.KEYWORD, "?")
                    tokens = self.cut_marked_tokens()
                    self.emit_tokens(tokens)
            else:
                if num_children_visited == 1:
                    self.emit_token(asttoken.KEYWORD, "if")
        else:
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

    def cond_else(self, node, num_children_visited, is_if_expr):
        if is_if_expr:
            if self.syntax.ternary_replaces_if_expr:
                if num_children_visited == 0:
                    self.emit_token(asttoken.KEYWORD, ":")
            else:
                if num_children_visited == 0:
                    self.emit_token(asttoken.KEYWORD_ELSE)
        else:
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

    def slice(self, node, num_children_visited):
        if num_children_visited == 1:
            self.emit_token(asttoken.VALUE_SEPARATOR, value=":")
            
    def subscript(self, node, num_children_visited):
        if num_children_visited == 1:
            self.emit_token(asttoken.SUBSCRIPT, is_start=True)
        elif num_children_visited == -1:
            self.emit_token(asttoken.SUBSCRIPT, is_start=False)

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
DIV_BINOP = BinOp("/", 2)
MULT_BINOP = BinOp("*", 2)


def _get_binop_for_node(node):
    if isinstance(node.op, ast.Add):
        return ADD_BINOP
    elif isinstance(node.op, ast.Div):
        return DIV_BINOP
    elif isinstance(node.op, ast.Mult):
        return MULT_BINOP
    else:
        assert False, "bad binop node %s" % node
