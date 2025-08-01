from lang.target import targets
from visitor import visitors
import ast
import types
import visitor.asttoken as asttoken
import visitor.nodeattrs as nodeattrs


class TokenVisitor(visitors._CommonStateVisitor):

    def __init__(self, ast_context, target):
        super().__init__(ast_context, target)
        self.binop_stack = []

        self.tokens = []

        # hack to handle no-args (== no children)
        self._funcdef_args_next = False

    def module(self, node, num_children_visited):
        super().module(node, num_children_visited)

    def imports(self, node, num_children_visited):
        super().imports(node, num_children_visited)
        if num_children_visited == 0:
            self.emit_token(asttoken.IMPORTS, is_start=True)
        elif num_children_visited == -1:
            self.emit_token(asttoken.IMPORTS, is_start=False)

    def import_stmt(self, node, num_children_visited):
        super().import_stmt(node, num_children_visited)
        if num_children_visited == 0:
            for alias_node in node.names:
                self.emit_token(asttoken.IMPORT, alias_node.name)

    def block(self, node, num_children_visited, is_root_block, body):
        token_type = asttoken.BLOCK
        is_python_lambda = targets.is_python(self.target) and isinstance(node, ast.Lambda)
        if is_python_lambda:
            # python only, probably no need to make generic
            token_type = asttoken.BLOCK_ON_SAME_LINE
        if not is_root_block:
            if num_children_visited == 0:
                self.emit_token(token_type, is_start=True)
            elif num_children_visited == -1:
                self.emit_token(token_type, is_start=False)

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
        if num_children_visited == 0:
            attrs = nodeattrs.get_attrs(node)
            # only deref?
            if nodeattrs.DEREF_NODE_MD in attrs:
                # foo.name -> *foo.name
                self.emit_token(asttoken.POINTER_DEREF)
        elif num_children_visited == -1:
            self.emit_token(asttoken.DOTOP)
            self.emit_token(asttoken.IDENTIFIER, node.attr)

    def call(self, node, num_children_visited):
        deref = nodeattrs.get_attr(node, nodeattrs.DEREF_NODE_MD, False)
        func = nodeattrs.get_function(node, must_exist=False)
        is_ctor = func is not None and func.is_constructor
        boundary_token = asttoken.CLASS_INST_BOUNDARY if is_ctor else asttoken.FUNC_CALL_BOUNDARY
        if num_children_visited == 0:
            if is_ctor:
                if self.target.object_instantiation_op is not None:
                    self.emit_token(asttoken.KEYWORD, self.target.object_instantiation_op)
            if deref:
                self.emit_token(asttoken.POINTER_DEREF)
            if self.target.is_prefix:
                self.emit_token(boundary_token, is_start=True)
        elif num_children_visited == 1:
            if not self.target.is_prefix:
                self.emit_token(boundary_token, is_start=True)
        elif num_children_visited > 1:
            self.emit_token(asttoken.FUNC_ARG, is_start=False)
        if num_children_visited == -1:
            self.emit_token(boundary_token, is_start=False)

    def constant(self, node, num_children_visited):
        super().constant(node, num_children_visited)
        self.emit_token(asttoken.LITERAL, node.value)

    def list_comp(self, node, num_children_visited):
        super().list_comp(node, num_children_visited)
        if num_children_visited == 0:
            self.emit_token(asttoken.CONTAINER_LITERAL_BOUNDARY,
                            value="[",
                            is_start=True)
        elif num_children_visited == 1:
            self.emit_token(asttoken.KEYWORD, "for")
        elif num_children_visited == -1:
            self.emit_token(asttoken.CONTAINER_LITERAL_BOUNDARY,
                            value="]",
                            is_start=False)

    def list_comp_generator(self, node, num_children_visited):
        super().list_comp_generator(node, num_children_visited)
        if num_children_visited == 1:
            self.emit_token(asttoken.KEYWORD, "in")
        elif num_children_visited == 2 and len(node.ifs) > 0:
            self.emit_token(asttoken.KEYWORD, "if")

    def loop_for(self, node, num_children_visited, is_foreach):
        super().loop_for(node, num_children_visited, is_foreach)
        if num_children_visited == 0:
            self.emit_token(asttoken.KEYWORD, "for")
            self.emit_token(asttoken.FLOW_CONTROL_TEST, is_start=True)
            if is_foreach:
                if isinstance(node.target, ast.Name):
                    if not self.target.dynamically_typed:
                        # TODO
                        # this hardcodes the type name in front of the for loop
                        # variable - this is Java specific
                        type_info = self.ast_context.lookup_type_info_by_node(node.target)
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

    def classdef(self, node, num_children_visited):
        super().classdef(node, num_children_visited)
        if num_children_visited == 0:
            self.emit_token(asttoken.CLASS_DEF, node.name)

    def funcarg(self, node, num_children_visited):
        super().funcarg(node, num_children_visited)
        self.emit_token(asttoken.FUNC_ARG, is_start=True)
        arg_type_info = self.ast_context.get_type_info_by_node(node)
        arg_type_name = self.target.type_mapper.lookup_target_type_name(arg_type_info)
        self.emit_token(asttoken.KEYWORD, arg_type_name)
        self.emit_token(asttoken.IDENTIFIER, node.arg)
        self.emit_token(asttoken.FUNC_ARG, is_start=False)

    def lambdadef(self, node, num_children_visited):
        super().lambdadef(node, num_children_visited)
        self._handle_funcdef(node, num_children_visited, is_anon=True)

    def funcdef(self, node, num_children_visited):
        super().funcdef(node, num_children_visited)
        self._handle_funcdef(node, num_children_visited, is_anon=False)

    def _handle_funcdef(self, node, num_children_visited, is_anon):
        if num_children_visited == 0 and not self._funcdef_args_next:
            self._funcdef_args_next = True
            scope = self.ast_context.current_scope.get()
            value = (scope, nodeattrs.get_attrs(node),)
            if is_anon:
                self.emit_token(asttoken.ANON_FUNC_DEF_BOUNDARY, value, is_start=True)
                self.emit_token(asttoken.FUNC_DEF, "lambda")
            else:
                self.emit_token(asttoken.FUNC_DEF_BOUNDARY, value, is_start=True)
                self.emit_token(asttoken.FUNC_DEF, node.name)
            func = nodeattrs.get_function(node)
            rtn_type_info = func.get_rtn_type_info()
            if rtn_type_info.value_type is types.NoneType:
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
            if is_anon:
                self.emit_token(asttoken.ANON_FUNC_DEF_BOUNDARY, is_start=False)
            else:
                self.emit_token(asttoken.FUNC_DEF_BOUNDARY, is_start=False)

    def name(self, node, num_children_visited):
        super().name(node, num_children_visited)
        ident = node.id
        attrs = nodeattrs.get_attrs(node)
        if nodeattrs.ADDRESS_OF_NODE_MD in attrs:
            ident = "&%s" % ident
        elif nodeattrs.DEREF_NODE_MD in attrs:
            ident = "*%s" % ident
        if nodeattrs.QUOTE_NODE_ATTR in attrs:
            ident = "'%s" % ident
        self.emit_token(asttoken.IDENTIFIER, ident)

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
            if type_mapping.value_separator is not None:
                # some languages (lisp ...) do not have a value sep
                self.emit_token(asttoken.SEPARATOR,
                                value=type_mapping.value_separator)

    def container_type_list(self, node, num_children_visited):
        super().container_type_list(node, num_children_visited)
        self._container_type_sequence(node, num_children_visited, list)

    def container_type_tuple(self, node, num_children_visited):
        super().container_type_list(node, num_children_visited)
        self._container_type_sequence(node, num_children_visited, tuple)

    def _container_type_sequence(self, node, num_children_visited, py_type):
        type_info = self.ast_context.get_type_info_by_node(node)
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
            if targets.is_python(self.target):
                # this python hack is ugly for sure. this is because python
                # doesn't return multiple values from a function, it wraps those
                # in a tuple - we could add another bool for this ... but so far
                # this is a python edge case
                write_literal_boundary = False
            else:
                scope = self.ast_context.current_scope.get()
                _, func_node = scope.get_enclosing_namespace()
                func = nodeattrs.get_function(func_node)
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
        start_literal = type_mapping.start_literal
        if not is_empty and type_mapping.start_values_wrapper is not None:
            start_literal += type_mapping.start_values_wrapper
        # replace $contained_type
        type_info = self.ast_context.lookup_type_info_by_node(node)
        start_literal = self.target.type_mapper.replace_contained_type(type_info, start_literal)
        return start_literal

    def _build_container_end_literal(self, node, is_empty, type_mapping):
        end_literal = type_mapping.end_literal
        if not is_empty and type_mapping.end_values_wrapper is not None:
            end_literal += type_mapping.end_values_wrapper
        return end_literal

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

    def pass_stmt(self, node, num_children_visited):
        super().pass_stmt(node, num_children_visited)
        if targets.is_python(self.target):
            # pass is python specific
            self.emit_token(asttoken.KEYWORD, "pass")

    def assign(self, node, num_children_visited):
        super().assign(node, num_children_visited)
        lhs = node.targets[0].get()
        scope = self.ast_context.current_scope.get()
        is_declaration = scope.is_declaration_node(lhs)
        type_decl_value = (scope, nodeattrs.get_attrs(node), node)
        if num_children_visited == 0:
            if is_declaration:
                self.emit_token(asttoken.TYPE_DECLARATION, type_decl_value, is_start=True)
            if not self.target.dynamically_typed:
                if is_declaration:
                    rhs = node.value
                    rhs_type_info = self.ast_context.lookup_type_info_by_node(rhs)
                    assert rhs_type_info is not None, "rhs type info is None for Node %s" % rhs_type_info
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
                        assert rhs_type_info.is_none_type or lhs_type_info.value_type == rhs_type_info.value_type, "type insanity, expected same type infos for lhs and rhs but got lhs: %s rhs: %s" % (lhs_type_info, rhs_type_info)
                        target_type_name = self.target.type_mapper.lookup_target_type_name(lhs_type_info)
                        assert target_type_name is not None
                        self.emit_token(asttoken.KEYWORD, target_type_name)
        elif num_children_visited == 1:
            if is_declaration:
                self.emit_token(asttoken.TYPE_DECLARATION, type_decl_value, is_start=False)
                self.emit_token(asttoken.TYPE_DECLARATION_RHS,
                                value=type_decl_value, is_start=True)
            else:
                self.emit_token(asttoken.BINOP, "=")
        else:
            if is_declaration:
                self.emit_token(asttoken.TYPE_DECLARATION_RHS,
                                value=type_decl_value, is_start=False)

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
        self.emit_token(asttoken.BINOP, self.target.eq_binop)

    def not_eq(self, node, num_children_visited):
        self.emit_token(asttoken.BINOP, self.target.not_eq_binop)

    def unary_not(self, node, num_children_visited):
        self.emit_token(asttoken.UNARYOP, self.target.not_unaryop)

    def same(self, node, num_children_visited):
        self.emit_token(asttoken.BINOP, self.target.same_binop)

    def not_same(self, node, num_children_visited):
        self.emit_token(asttoken.BINOP, self.target.not_same_binop)

    def less_than(self, node, num_children_visited):
        # prototype - generalize - so we need a method called for all
        # node types after all ...
        quote = nodeattrs.get_attr(node, nodeattrs.QUOTE_NODE_ATTR, False)
        value = "'<" if quote else "<"
        self.emit_token(asttoken.BINOP, value)

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
        deref = nodeattrs.get_attr(node, nodeattrs.DEREF_NODE_MD, False)
        if num_children_visited == 0:
            if deref:
                self.emit_token(asttoken.POINTER_DEREF, "(")
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
