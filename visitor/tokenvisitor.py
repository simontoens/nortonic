import ast

from visitor import visitors
import asttoken
import context
import nodeattrs
import nodes


class TokenVisitor(visitors._CommonStateVisitor):

    def __init__(self, ast_context, target):
        super().__init__(ast_context, target)
        self.ast_context = ast_context
        self.target = target
        self.binop_stack = []

        self.tokens = []

        # hack to handle no-args (== no children)
        self._funcdef_args_next = False

    def block(self, node, num_children_visited, is_root_block, body):
        if not is_root_block:
            if num_children_visited == 0:
                self.emit_token(asttoken.BLOCK, is_start=True)
            elif num_children_visited == -1:
                self.emit_token(asttoken.BLOCK, is_start=False)

    def stmt(self, node, num_children_visited, parent_node, is_last_body_stmt):
        token_type = asttoken.BODY_STMT if hasattr(node, "body") else asttoken.STMT
        if num_children_visited == 0:
            self.emit_token(token_type, is_start=True)
        elif num_children_visited == -1:
            if isinstance(parent_node, ast.FunctionDef) and is_last_body_stmt:
                if self.target.function_signature_template is not None:
                    delim = self.target.function_signature_template\
                        .get_function_body_end_delim()
                    if delim is not None:
                         self.emit_token(asttoken.CUSTOM_FUNCDEF_END_BODY_DELIM,
                                         delim)
            self.emit_token(token_type, is_start=False)

    def attr(self, node, num_children_visited):
        if num_children_visited == -1:
            self.emit_token(asttoken.TARGET_DEREF)
            self.emit_token(asttoken.IDENTIFIER, node.attr)

    def call(self, node, num_children_visited):
        emit_boundary_tokens = not isinstance(node, nodes.CallAsKeyword)
        if num_children_visited == 0:
            if self.target.is_prefix:
                if emit_boundary_tokens:
                    self.emit_token(asttoken.FUNC_CALL_BOUNDARY, is_start=True)
        elif num_children_visited == 1:
            if not self.target.is_prefix:
                if emit_boundary_tokens:
                    self.emit_token(asttoken.FUNC_CALL_BOUNDARY, is_start=True)
        elif num_children_visited > 1:
            if emit_boundary_tokens:
                self.emit_token(asttoken.FUNC_ARG, is_start=False)
        if num_children_visited == -1:
            if emit_boundary_tokens:
                self.emit_token(asttoken.FUNC_CALL_BOUNDARY, is_start=False)

    def constant(self, node, num_children_visited):
        super().constant(node, num_children_visited)
        self.emit_token(asttoken.LITERAL, node.value)

    def loop_for(self, node, num_children_visited, is_foreach):
        super().loop_for(node, num_children_visited, is_foreach)
        if num_children_visited == 0:
            self.emit_token(asttoken.KEYWORD, "for")
            self.emit_token(asttoken.FLOW_CONTROL_TEST, is_start=True)
            if is_foreach:
                if self.target.strongly_typed:
                    # this hardcodes the type name in front of the for loop
                    # variable - this is ok for Java
                    if isinstance(node.target.get(), ast.Name):
                        type_info = self.ast_context.lookup_type_info_by_node(node.target.get())
                        target_type_name = self.target.type_mapper.lookup_target_type_name(type_info)
                        self.emit_token(asttoken.KEYWORD, target_type_name)
        if is_foreach:
            if num_children_visited == 1:
                self.emit_token(asttoken.KEYWORD, self.target.loop_foreach_keyword)
            elif num_children_visited == 2:
                self.emit_token(asttoken.FLOW_CONTROL_TEST, is_start=False)
        else:
            if num_children_visited in (1,2):
                self.emit_token(asttoken.SEPARATOR, self.target.stmt_end_delim)
            elif num_children_visited == 3:
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
            self.emit_token(asttoken.FUNC_DEF_BOUNDARY, scope, is_start=True)
            self.emit_token(asttoken.FUNC_DEF, node.name)
            if self.target.strongly_typed:
                func = nodeattrs.get_function(node)
                rtn_type_info = func.get_rtn_type_info()
                if rtn_type_info.value_type == None.__class__:
                    # method does not return anything, ie void
                    pass
                else:
                    if func.returns_multiple_values(self.target):
                        # pass through the contained types, assumes golang
                        # syntax until another one is needed
                        rtn_type_name = "(%s)" % self.target.type_mapper.lookup_contained_type_names(rtn_type_info, sep=", ")
                    else:
                        rtn_type_name = self.target.type_mapper.lookup_target_type_name(rtn_type_info)
                    self.emit_token(asttoken.KEYWORD_RTN, rtn_type_name)
        elif self._funcdef_args_next:
            self._funcdef_args_next = False
            self.emit_token(asttoken.FUNC_DEF_BOUNDARY, is_start=False)

    def name(self, node, num_children_visited):
        ident = node.id
        if self.target.strongly_typed:
            metadata = node.get_node_metadata()
            if metadata.get(nodeattrs.ADDRESS_OF_NODE_MD):
                ident = "&%s" % ident
            elif metadata.get(nodeattrs.DEREF_NODE_MD):
                ident = "*%s" % ident
        self.emit_token(asttoken.IDENTIFIER, ident)

    def num(self, node, num_children_visited):
        self.emit_token(asttoken.LITERAL, node.n)

    def container_type_dict(self, node, num_children_visited):
        super().container_type_dict(node, num_children_visited)
        type_info = self.ast_context.lookup_type_info_by_node(node)
        type_mapping = self.target.type_mapper.get_type_mapping(type_info)
        is_empty = len(node.keys) == 0
        if num_children_visited == 0:
            start = self._build_container_start_literal(node, is_empty, type_mapping)
            self.emit_token(asttoken.CONTAINER_LITERAL_BOUNDARY,
                            value=start,
                            is_start=True)
        elif num_children_visited == -1:
            end = self._build_container_end_literal(node, is_empty, type_mapping)
            self.emit_token(asttoken.CONTAINER_LITERAL_BOUNDARY,
                            value=end,
                            is_start=False)
        elif num_children_visited % 2 == 0:
            if num_children_visited / 2 < len(node.keys):
                self.emit_token(asttoken.FUNC_ARG, is_start=False)
        else: # num_children_visited > 0:
            self.emit_token(asttoken.SEPARATOR,
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
        write_literal_boundary = True
        if self.assign_visiting_lhs:
            # unpacking
            write_literal_boundary = False
        elif self.loop_visiting_lhs:
            # unpacking
            write_literal_boundary = False
        elif self.visiting_rtn:
            # generate:
            #     def foo():
            #         return 1, 2
            # instead of:
            #     def foo():
            #         return (1, 2)
            if "python" in str(type(self.target)):
                # this python hack is ugly for sure. this is because python
                # doesn't return multiple values from a function, it wraps those
                # in a tuple - we could add another bool for this ... but so far
                # this is a python edge case
                write_literal_boundary = False
            else:
                scope = self.ast_context.current_scope.get()
                func = nodeattrs.get_function(scope.ast_node)
                func_returns_mult = func.returns_multiple_values(self.target)
                if func_returns_mult:
                    write_literal_boundary = False
        if write_literal_boundary:
            is_empty = len(node.elts) == 0
            if num_children_visited == 0:
                start = self._build_container_start_literal(node, is_empty, type_mapping)
                self.emit_token(asttoken.CONTAINER_LITERAL_BOUNDARY,
                                value=start,
                                is_start=True)
            elif num_children_visited == -1:
                end = self._build_container_end_literal(node, is_empty, type_mapping)
                self.emit_token(asttoken.CONTAINER_LITERAL_BOUNDARY,
                                value=end,
                                is_start=False)
        if num_children_visited > 0:
            if num_children_visited < len(node.elts):
                # list literal arguments look like function arguments
                self.emit_token(asttoken.FUNC_ARG, is_start=False)

    def _build_container_start_literal(self, node, is_empty, type_mapping):
        l = type_mapping.start_literal
        if not is_empty and type_mapping.start_values_wrapper is not None:
            l += type_mapping.start_values_wrapper
        if self.target.strongly_typed:
            # replace $contained_type
            type_info = self.ast_context.lookup_type_info_by_node(node)
            l = self.target.type_mapper.replace_contained_type(type_info, l)
        return l

    def _build_container_end_literal(self, node, is_empty, type_mapping):
        l = type_mapping.end_literal
        if not is_empty and type_mapping.end_values_wrapper is not None:
            l += type_mapping.end_values_wrapper
        return l

    def string(self, node, num_children_visited):
        self.emit_token(asttoken.LITERAL, node.s)

    def emit_token(self, type, value=None, is_start=None, id=None):
        self.tokens.append(asttoken.Token(value, type, is_start))

    def emit_tokens(self, tokens):
        self.tokens.extend(tokens)

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

    def boolop_and(self, node, num_children_visited):
        self._emit_binop(node)

    def boolop_or(self, node, num_children_visited):
        self._emit_binop(node)
            
    def add(self, node, num_children_visited):
        self._emit_binop(node)

    def uadd(self, node, num_children_visited):
        self.emit_token(asttoken.UNARYOP, "+")

    def sub(self, node, num_children_visited):
        self._emit_binop(node)

    def usub(self, node, num_children_visited):
        self.emit_token(asttoken.UNARYOP, "-")

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
                # - the scope
                # - the node metadata
                value = (scope, node.get_node_metadata())
                self.emit_token(asttoken.TYPE_DECLARATION, value, is_start=True)
            if self.target.strongly_typed:
                if is_declaration:
                    rhs = node.value
                    rhs_type_info = self.ast_context.lookup_type_info_by_node(rhs)
                    assert rhs_type_info is not None, "rhs type info is None"
                    lhs_nodes = [lhs]
                    if isinstance(lhs, ast.Tuple):
                        # unpacking: a,b = (1, 2)
                        # logic below needs to be adjusted to handle this case
                        pass
                    else:
                        lhs_type_info = self.ast_context.lookup_type_info_by_node(lhs)
                        assert lhs_type_info is not None, "lhs type info is None for %s" % lhs

                        # rhs_type_info.is_none_type may happen if
                        # a = None
                        # a = 1
                        # then, the declaration node is "a = None"
                        assert rhs_type_info.is_none_type or lhs_type_info == rhs_type_info, "type insanity, expected same type infos for lhs and rhs but got lhs: %s rhs: %s" % (lhs_type_info, rhs_type_info)
                        target_type_name = self.target.type_mapper.lookup_target_type_name(lhs_type_info)
                        assert target_type_name is not None
                        self.emit_token(asttoken.KEYWORD, target_type_name)
        elif num_children_visited == 1:
            if is_declaration:
                self.emit_token(asttoken.TYPE_DECLARATION, is_start=False)
            else:
                self.emit_token(asttoken.BINOP, "=")

    def assign_aug(self, node, num_children_visited):
        super().assign_aug(node, num_children_visited)
        if num_children_visited == 2:
            self.emit_token(asttoken.BINOP, "=")

    def cond_if(self, node, num_children_visited):
        if num_children_visited == 0:
            self.emit_token(asttoken.KEYWORD, "if")
            self.emit_token(asttoken.FLOW_CONTROL_TEST, is_start=True)
        elif num_children_visited == 1:
            self.emit_token(asttoken.FLOW_CONTROL_TEST, is_start=False)

    def cond_else(self, node, num_children_visited):
        if num_children_visited == 0:
            self.emit_token(asttoken.KEYWORD_ELSE)

    def cond_if_expr(self, node, num_children_visited):
        if self.target.ternary_replaces_if_expr:
            if num_children_visited == 1:
                self.emit_token(asttoken.KEYWORD, "?")
        else:
            if num_children_visited == 1:
                self.emit_token(asttoken.KEYWORD, "if")

    def cond_if_expr_else(self, node, num_children_visited):
            if self.target.ternary_replaces_if_expr:
                if num_children_visited == 0:
                    self.emit_token(asttoken.KEYWORD, ":")
            else:
                if num_children_visited == 0:
                    self.emit_token(asttoken.KEYWORD_ELSE)

    def eq(self, node, num_children_visited):
        self.emit_token(asttoken.BINOP, self.target.equality_binop)

    def identity(self, node, num_children_visited):
        self.emit_token(asttoken.BINOP, self.target.identity_binop)

    def less_than(self, node, num_children_visited):
        self.emit_token(asttoken.BINOP, "<")

    def greater_than(self, node, num_children_visited):
        self.emit_token(asttoken.BINOP, ">")

    def rtn(self, node, num_children_visited):
        super().rtn(node, num_children_visited)
        if num_children_visited == 0:
            self.emit_token(asttoken.KEYWORD_RTN)

    def slice(self, node, num_children_visited):
        if num_children_visited == 1:
            self.emit_token(asttoken.SEPARATOR, value=":")
            
    def subscript(self, node, num_children_visited):
        deref = node.get_node_metadata().get(nodeattrs.DEREF_NODE_MD)
        if num_children_visited == 0:
            if deref:
                self.emit_token(asttoken.POINTER_DEREF, "(*")
        elif num_children_visited == 1:
            if deref:
                self.emit_token(asttoken.POINTER_DEREF, ")")
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
