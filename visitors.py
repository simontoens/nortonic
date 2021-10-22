import ast
import context
import nodeattrs
import syntax
import transformer
import visitor


class FuncCallVisitor(visitor.NoopNodeVisitor):
    """
    Hooks to rewrite function calls.
    """

    def __init__(self, ast_context, syntax):
        self.ast_context = ast_context
        self.syntax = syntax

    def assign(self, node, num_children_visited):
        if num_children_visited == -1:
            assert len(node.targets) == 1
            # use '=' to transform into a function call
            self._handle_function_call("=", node, arg_nodes=[node.targets[0], node.value])

    def binop(self, node, num_children_visited):
        if num_children_visited == -1:
            if isinstance(node.op, ast.Add):
                op = "+"
            elif isinstance(node.op, ast.Mult):
                op = "*"
            else:
                assert False, "Unhandled binop"
            self._handle_function_call(op, node, [node.left, node.right])

    def compare(self, node, num_children_visited):
        if num_children_visited == -1:
            assert len(node.ops) == 1
            assert len(node.comparators) == 1            
            if isinstance(node.ops[0], ast.Eq):
                op = "=="
            else:
                assert False, "Unhandled comparison %s" % node.ops[0]
            self._handle_function_call(op, node, [node.left, node.comparators[0]])

    def call(self, node, num_children_visited):
        if num_children_visited == -1:
            self._handle_function_call(node.func.id, node, node.args)

    def cond_if(self, node, num_children_visited):
        if num_children_visited == -1:
            # use 'if' to transform into a function call
            self._handle_function_call("if", node, arg_nodes=[node.test])

    def _handle_function_call(self, func_name, node, arg_nodes):
        if func_name in self.syntax.functions:
            args = []
            for arg_node in arg_nodes:
                type_info = self.ast_context.lookup_type_info_by_node(arg_node)
                assert type_info is not None, "unable to lookup type info for function %s: arg %s" % (func_name, arg_node)
                args.append(syntax.Argument(arg_node, type_info.value_type))
            tr = transformer.ASTTransformer(node, arg_nodes, self.ast_context)
            func = self.syntax.functions[func_name]
            func.rewrite(args=args, ast_transformer=tr)


class TypeVisitor(visitor.NoopNodeVisitor):
    """
    This visitor determines the type of every AST Node.
    """

    def __init__(self, ast_context):
        self.ast_context = ast_context
        self.visiting_lhs = False
        self.visiting_rhs = False
        self.lhs_value = None
        self.id_name_to_type_info = {}

    def assign(self, node, num_children_visited):
        if num_children_visited == 0:
            assert self.visiting_lhs == False
            assert self.visiting_rhs == False
            self.visiting_lhs = True
        else:
            self.visiting_lhs = False
            self.visiting_rhs = True
            if num_children_visited == -1:
                self.visiting_rhs = False
                # add mapping of lhs id name -> to its type
                type_info = self.ast_context.lookup_type_info_by_node(node.value)
                assert type_info is not None, "Unable to lookup type of assignment rhs"
                self.id_name_to_type_info[self.lhs_value] = type_info
                assert len(node.targets) == 1
                self.ast_context.register_type_info_by_node(node.targets[0], type_info)

    def binop(self, node, num_children_visited):
        if num_children_visited == -1:
            self._register_node_target_type(node, node.left, node.right)

    def compare(self, node, num_children_visited):
        if num_children_visited == -1:
            assert len(node.comparators) == 1
            self._register_node_target_type(node, node.left, node.comparators[0])
        
    def name(self, node, num_children_visited):
        if self.visiting_lhs:
            self.lhs_value = node.id
        else:
            # a = b, lookup b's type
            type_info = self.id_name_to_type_info.get(node.id, None)
            assert type_info is not None, "Cannot find type info for '%s'" % node.id
            self.ast_context.register_type_info_by_node(node, type_info)

    def num(self, node, num_children_visited):
        self._register_type(node, node.n)

    def string(self, node, num_children_visited):
        self._register_type(node, node.s)

    def constant(self, node, num_children_visited):
        self._register_type(node, node.value)

    def _register_type(self, node, value):
        type_info = context.TypeInfo(type(value))
        self.ast_context.register_type_info_by_node(node, type_info)

    def _register_node_target_type(self, target_node, lhs_node, rhs_node):
        lhs_type_info = self.ast_context.lookup_type_info_by_node(lhs_node)
        assert lhs_type_info is not None, "Unable to lookup LHS node type"
        rhs_type_info = self.ast_context.lookup_type_info_by_node(rhs_node)
        assert rhs_type_info is not None, "Unable to find RHS node type"

        # FIXME needs to move onto syntax
        # Python does not support string + num type coercion, for
        # example these expressions are not valid: 1 + "foo", "foo" + 1
        lt = lhs_type_info.value_type
        rt = rhs_type_info.value_type
        if lt is float or rt is float:
            result_type_info = context.TypeInfo(float)
        elif lt is str or rt is str:
            result_type_info = context.TypeInfo(str)
        else:
            # FIXME - bool etc
            result_type_info = context.TypeInfo(int)
        self.ast_context.register_type_info_by_node(target_node, result_type_info)
