import ast
import asttoken
import nodeattrs
import visitor
import visitors


_START_MARK = "START_MARK"
_END_MARK = "END_MARK"


class TokenVisitor(visitors._CommonStateVisitor):

    def __init__(self, ast_context, target):
        super().__init__(ast_context, target)
        self.ast_context = ast_context
        self.target = target
        self.binop_stack = []

        self.tokens = []

        # hack to handle no-args (== no children)
        self._funcdef_args_next = False

    def block(self, node, num_children_visited):
        if num_children_visited == 0:
            self.emit_token(asttoken.BLOCK, is_start=True)
        elif num_children_visited == -1:
            self.emit_token(asttoken.BLOCK, is_start=False)

    def stmt(self, node, num_children_visited):
        token_type = asttoken.BODY_STMT if hasattr(node, "body") else asttoken.STMT
        if num_children_visited == 0:
            self.emit_token(token_type, is_start=True)
        elif num_children_visited == -1:
            self.emit_token(token_type, is_start=False)

    def attr(self, node, num_children_visited):
        if num_children_visited == -1:
            self.emit_token(asttoken.TARGET_DEREF)
            self.emit_token(asttoken.IDENTIFIER, node.attr)

    def call(self, node, num_children_visited):
        if num_children_visited == 0:
            if self.target.is_prefix:
                self.emit_token(asttoken.FUNC_CALL_BOUNDARY, is_start=True)
        elif num_children_visited == 1:
            if not self.target.is_prefix:
                self.emit_token(asttoken.FUNC_CALL_BOUNDARY, is_start=True)
        elif num_children_visited > 1:
            self.emit_token(asttoken.FUNC_ARG, is_start=False)
        if num_children_visited == -1:
            self.emit_token(asttoken.FUNC_CALL_BOUNDARY, is_start=False)

    def constant(self, node, num_children_visited):
        super().constant(node, num_children_visited)
        self.emit_token(asttoken.LITERAL, node.value)

    def loop_for(self, node, num_children_visited):
        super().loop_for(node, num_children_visited)
        if num_children_visited == 0:
            self.emit_token(asttoken.KEYWORD, "for")
            self.emit_token(asttoken.FLOW_CONTROL_TEST, is_start=True)
            if self.target.strongly_typed:
                type_info = self.ast_context.lookup_type_info_by_node(node.target)
                target_type_name = self.target.type_mapper.lookup_target_type_name(type_info)
                self.emit_token(asttoken.KEYWORD, target_type_name)
        elif num_children_visited == 1:
            self.emit_token(asttoken.KEYWORD, self.target.loop_foreach_keyword)
        elif num_children_visited == 2:
            self.emit_token(asttoken.FLOW_CONTROL_TEST, is_start=False)

    def loop_continue(self, node, num_children_visited):
        super().loop_continue(node, num_children_visited)
        self.emit_token(asttoken.KEYWORD, "continue")

    def loop_break(self, node, num_children_visited):
        super().loop_continue(node, num_children_visited)
        self.emit_token(asttoken.KEYWORD, "break")

    def funcarg(self, node, num_children_visited):
        super().funcarg(node, num_children_visited)
        type_info = self.ast_context.lookup_type_info_by_node(node)
        self.emit_token(asttoken.FUNC_ARG, is_start=True)
        if self.target.strongly_typed:
            arg_type_info = self.ast_context.lookup_type_info_by_node(node)
            arg_type_name = self.target.type_mapper.lookup_target_type_name(arg_type_info)            
            self.emit_token(asttoken.KEYWORD, arg_type_name)
        self.emit_token(asttoken.IDENTIFIER, node.arg)
        self.emit_token(asttoken.FUNC_ARG, is_start=False)

    def funcdef(self, node, num_children_visited):
        if num_children_visited == 0 and not self._funcdef_args_next:
            self._funcdef_args_next = True
            scope = self.ast_context.current_scope.get()
            self.emit_token(asttoken.FUNC_DEF_BOUNDARY, scope.owner, is_start=True)
            self.emit_token(asttoken.FUNC_DEF, node.name)
            if self.target.strongly_typed:
                func = self.ast_context.get_function(node.name)
                rtn_type_info = func.get_rtn_type_info()
                if rtn_type_info.value_type == None.__class__:
                    # method does not return anything, ie void
                    pass
                else:
                    rtn_type_name = self.target.type_mapper.lookup_target_type_name(rtn_type_info)
                    # hacky (?) way to pass through the return type
                    self.emit_token(asttoken.KEYWORD_RTN, rtn_type_name)
        elif self._funcdef_args_next:
            self._funcdef_args_next = False
            self.emit_token(asttoken.FUNC_DEF_BOUNDARY, is_start=False)

    def name(self, node, num_children_visited):
        self.emit_token(asttoken.IDENTIFIER, node.id)

    def num(self, node, num_children_visited):
        self.emit_token(asttoken.LITERAL, node.n)

    def container_type_dict(self, node, num_children_visited):
        super().container_type_dict(node, num_children_visited)
        type_info = self.ast_context.lookup_type_info_by_node(node)
        type_mapping = self.target.type_mapper.get_type_mapping(type_info)
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
        type_mapping = self.target.type_mapper.get_type_mapping(type_info)
        if self.assign_visiting_lhs:
            # unpacking
            pass
        else:
            if num_children_visited == 0:
                start = self._build_container_start_literal(node, type_mapping)
                self.emit_token(asttoken.CONTAINER_LITERAL_BOUNDARY,
                                value=start,
                                is_start=True)
            elif num_children_visited == -1:
                end = self._build_container_end_literal(node, type_mapping)
                self.emit_token(asttoken.CONTAINER_LITERAL_BOUNDARY,
                                value=end,
                                is_start=False)
        if num_children_visited > 0:
            if num_children_visited < len(node.elts):
                # list literal arguments look like function arguments
                self.emit_token(asttoken.FUNC_ARG, is_start=False)

    def _build_container_start_literal(self, node, type_mapping):
        l = type_mapping.start_literal
        if len(node.elts) > 0 and type_mapping.start_values_wrapper is not None:
            l += type_mapping.start_values_wrapper
        if self.target.strongly_typed:
            # replace $contained_type - needs to be done properly when there
            # are multiple contained types
            type_info = self.ast_context.lookup_type_info_by_node(node)
            contained_type_names = self.target.type_mapper.lookup_contained_type_names(type_info)
            l = l.replace("$contained_type", contained_type_names[0])
        return l

    def _build_container_end_literal(self, node, type_mapping):
        l = type_mapping.end_literal
        if len(node.elts) > 0 and type_mapping.end_values_wrapper is not None:
            l += type_mapping.end_values_wrapper
        return l

    def string(self, node, num_children_visited):
        self.emit_token(asttoken.LITERAL, node.s)

    def emit_token(self, type, value=None, is_start=None, id=None):
        self.tokens.append(asttoken.Token(value, type, is_start))

    def emit_tokens(self, tokens):
        self.tokens.extend(tokens)

    def start_token_mark(self):
        self.tokens.append(_START_MARK)

    def end_token_mark(self):
        self.tokens.append(_END_MARK)

    def cut_marked_tokens(self):
        end_mark_index = -1
        for i in range(len(self.tokens) - 1, 0, -1):
            if end_mark_index == -1:
                if self.tokens[i] == _END_MARK:
                    end_mark_index = i
            else:
                if self.tokens[i] == _START_MARK:
                    cut_tokens = self.tokens[i+1:end_mark_index]
                    self.tokens = self.tokens[0:i] + self.tokens[end_mark_index+1:]
                    return cut_tokens
        raise Exception("nothing to cut")

    def binop(self, node, num_children_visited):
        super().binop(node, num_children_visited)
        self._handle_binop(node, num_children_visited)

    def _handle_binop(self, node, num_children_visited):
        binop = _get_binop_for_node(node.op, self.target)
        if num_children_visited == 0:
            self.binop_stack.append(binop)
            if self._binop_arg_requires_parens():
                self.emit_token(asttoken.BINOP_PREC_BIND, is_start=True)
        elif num_children_visited == -1:
            if self._binop_arg_requires_parens():
                self.emit_token(asttoken.BINOP_PREC_BIND, is_start=False)
            self.binop_stack.pop()

    def _binop_arg_requires_parens(self):
        if len(self.binop_stack) > 1:
            current_op = self.binop_stack[-1]
            parent_op = self.binop_stack[-2]
            if current_op.precedence < parent_op.precedence:
                return True
        return False

    def boolop(self, node, num_children_visited):
        super().boolop(node, num_children_visited)
        self._handle_binop(node, num_children_visited)

    def unaryop(self, node, num_children_visited):
        if num_children_visited == 0:
            assert isinstance(node.op, ast.USub), node.op
            self.emit_token(asttoken.UNARYOP, "-")

    def boolop_and(self, node, num_children_visited):
        self._emit_binop(node)

    def boolop_or(self, node, num_children_visited):
        self._emit_binop(node)
            
    def add(self, node, num_children_visited):
        self._emit_binop(node)

    def sub(self, node, num_children_visited):
        self._emit_binop(node)

    def div(self, node, num_children_visited):
        self._emit_binop(node)

    def mult(self, node, num_children_visited):
        self._emit_binop(node)

    def mod(self, node, num_children_visited):
        self._emit_binop(node)

    def _emit_binop(self, op_node):
        b = _get_binop_for_node(op_node, self.target)
        self.emit_token(asttoken.BINOP, b.op)

    def assign(self, node, num_children_visited):
        super().assign(node, num_children_visited)
        lhs = node.targets[0].get()
        scope = self.ast_context.current_scope.get()
        is_declaration = scope.is_declaration_node(lhs)
        if num_children_visited == 0:
            if is_declaration:
                # we pass a few things through as value here, as a tuple:
                # - the scope owner (method, module function etc)
                # - the node metadata
                value = (scope.owner, node.get_metadata())
                self.emit_token(asttoken.TYPE_DECLARATION, value, is_start=True)
            if self.target.strongly_typed:
                if is_declaration:
                    lhs_type_info = self.ast_context.lookup_type_info_by_node(lhs)
                    assert lhs_type_info is not None, "lhs type info is None for %s" % lhs
                    rhs = node.value
                    rhs_type_info = self.ast_context.lookup_type_info_by_node(rhs)
                    assert rhs_type_info is not None, "rhs type info is None"
                    assert lhs_type_info == rhs_type_info, "type insanity"
                    target_type_name = self.target.type_mapper.lookup_target_type_name(lhs_type_info)
                    if target_type_name is None:
                        # this happens if the rhs of the assignment is None
                        # for example
                        # a=None
                        # check if a is ever given another value
                        # TODO check for mixed type assignemnts (and fail)?
                        for other_lhs in scope.get_ident_nodes_by_name(lhs.id):
                            lhs_type_info = self.ast_context.lookup_type_info_by_node(other_lhs)
                            target_type_name = self.target.type_mapper.lookup_target_type_name(lhs_type_info)
                            if target_type_name is not None:
                                break
                        else:
                            raise Exception("Unable to determine type of ident [%s]" % lhs.id)

                    self.emit_token(asttoken.KEYWORD, target_type_name)
        elif num_children_visited == 1:
            if is_declaration:
                self.emit_token(asttoken.TYPE_DECLARATION, is_start=False)
            else:
                self.emit_token(asttoken.BINOP, "=")

    def cond_if(self, node, num_children_visited, is_expr):
        if is_expr:
            if self.target.ternary_replaces_if_expr:
                if num_children_visited == 0:
                    # capture "body" and replay it after the conditional test
                    # has been emitted
                    # another (more elegant?) approach would be to support
                    # a way to specify a custom ast traversal order
                    # test -> if-branch -> else vs python's if-branch test else
                    self.start_token_mark()
                elif num_children_visited == 1:
                    self.end_token_mark()
                elif num_children_visited == 2:
                    self.emit_token(asttoken.KEYWORD, "?")
                    tokens = self.cut_marked_tokens()
                    self.emit_tokens(tokens)
            else:
                if num_children_visited == 1:
                    self.emit_token(asttoken.KEYWORD, "if")
        else:
            if num_children_visited == 0:
                self.emit_token(asttoken.KEYWORD, "if")
                self.emit_token(asttoken.FLOW_CONTROL_TEST, is_start=True)
            elif num_children_visited == 1:
                self.emit_token(asttoken.FLOW_CONTROL_TEST, is_start=False)

    def cond_else(self, node, num_children_visited, is_if_expr):
        if is_if_expr:
            if self.target.ternary_replaces_if_expr:
                if num_children_visited == 0:
                    self.emit_token(asttoken.KEYWORD, ":")
            else:
                if num_children_visited == 0:
                    self.emit_token(asttoken.KEYWORD_ELSE)
        else:
            if num_children_visited == 0:
                self.emit_token(asttoken.KEYWORD_ELSE)

    def eq(self, node, num_children_visited):
        self.emit_token(asttoken.BINOP, self.target.equality_binop)

    def identity(self, node, num_children_visited):
        self.emit_token(asttoken.BINOP, self.target.identity_binop)

    def rtn(self, node, num_children_visited):
        if num_children_visited == 0:
            self.emit_token(asttoken.KEYWORD_RTN)

    def slice(self, node, num_children_visited):
        if num_children_visited == 1:
            self.emit_token(asttoken.VALUE_SEPARATOR, value=":")
            
    def subscript(self, node, num_children_visited):
        if num_children_visited == 1:
            self.emit_token(asttoken.SUBSCRIPT, is_start=True)
        elif num_children_visited == -1:
            self.emit_token(asttoken.SUBSCRIPT, is_start=False)


class BinOp:

    def __init__(self, op, precedence):
        self.op = op
        self.precedence = precedence

    def __str__(self):
        return self.op


ADD_BINOP = BinOp("+", 1)
SUB_BINOP = BinOp("-", 1)
DIV_BINOP = BinOp("/", 2)
MULT_BINOP = BinOp("*", 2)
MOD_BINOP = BinOp("%", 2)


def _get_binop_for_node(op_node, target):
    if isinstance(op_node, ast.Add):
        return ADD_BINOP
    if isinstance(op_node, ast.Sub):
        return SUB_BINOP
    elif isinstance(op_node, ast.Div):
        return DIV_BINOP
    elif isinstance(op_node, ast.Mult):
        return MULT_BINOP
    elif isinstance(op_node, ast.Mod):
        return MOD_BINOP
    elif isinstance(op_node, ast.And):
        return BinOp(target.and_binop, 2)
    elif isinstance(op_node, ast.Or):
        return BinOp(target.or_binop, 1)
    else:
        assert False, "unsupported binop node %s" % op_node
