import ast
import context
import nodeattrs
import syntax
import transformer
import visitor


class _CommonStateVisitor(visitor.NoopNodeVisitor):

    def __init__(self, ast_context, syntax):
        self.ast_context = ast_context
        self.syntax = syntax

        self.visiting_func = False

    def call(self, node, num_children_visited):
        if num_children_visited == 0:
            assert self.visiting_func == False
            self.visiting_func = True
        elif num_children_visited == 1:
            assert self.visiting_func == True
            self.visiting_func = False

class FuncCallVisitor(_CommonStateVisitor):
    """
    Hooks to rewrite function calls.
    """

    def __init__(self, ast_context, syntax):
        super().__init__(ast_context, syntax)

        # needs to be a stack for nested func names: print("foo".startswith("f"))
        self.func_name_stack = []

    def assign(self, node, num_children_visited):
        if num_children_visited == -1:
            assert len(node.targets) == 1
            # use '=' to transform into a function call
            self._handle_function_call("=", node, arg_nodes=[node.targets[0], node.value])

    def attr(self, node, num_children_visited):
        assert self.visiting_func
        if num_children_visited == -1:
            self.func_name_stack.append(node.attr)

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
        super().call(node, num_children_visited)
        if num_children_visited == -1:
            func_name = self.func_name_stack.pop()
            self._handle_function_call(func_name, node, node.args)

    def cond_if(self, node, num_children_visited):
        if num_children_visited == -1:
            # use 'if' to transform into a function call
            self._handle_function_call("if", node, arg_nodes=[node.test])

    def name(self, node, num_children_visited):
        if self.visiting_func:
            self.func_name_stack.append(node.id)

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


class TypeVisitor(_CommonStateVisitor):
    """
    This visitor determines the type of every AST Node.
    """

    def __init__(self, ast_context, syntax):
        super().__init__(ast_context, syntax)

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
        if self.visiting_func:
            # todo - we'll need to record the type the func returns
            pass
        elif self.visiting_lhs:
            self.lhs_value = node.id
        else:
            # a = b or print(b) or any other b ref - lookup b's type
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
        target_type = self.syntax.combine_types(lhs_type_info.value_type,
                                                rhs_type_info.value_type)
        target_type_info = context.TypeInfo(target_type)
        self.ast_context.register_type_info_by_node(target_node, target_type_info)


class NodeDebugVisitor(visitor.NoopNodeVisitor):

    def add(self, node, num_children_visited):
        print(node)

    def binop(self, node, num_children_visited):
        print(node)

    def assign(self, node, num_children_visited):
        print(node)

    def call(self, node, num_children_visited):
        if num_children_visited == 0:
            print(node)
            print("ast.Call.func:", node.func)

    def compare(self, node, num_children_visited):
        print(node)

    def cond_if(self, node, num_children_visited):
        print(node)

    def cond_else(self, node, num_children_visited):
        print(node)

    def constant(self, node, num_children_visited):
        print(node)

    def eq(self, node, num_children_visited):
        print(node)

    def expr(self, node, num_children_visited):
        print(node)

    def funcdef(self, node, num_children_visited):
        print(node)

    def module(self, node, num_children_visited):
        print(node)

    def mult(self, node, num_children_visited):
        print(node)

    def name(self, node, num_children_visited):
        print(node)

    def name_constant(self, node, num_children_visited):
        print(node)

    def num(self, node, num_children_visited):
        print(node)

    def rtn(self, node, num_children_visited):
        print(node)

    def string(self, node, num_children_visited):
        print(node)
