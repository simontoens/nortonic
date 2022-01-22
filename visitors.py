import ast
import context
import nodeattrs
import pybuiltins
import syntax
import astrewriter
import visitor


class _CommonStateVisitor(visitor.NoopNodeVisitor):

    def __init__(self, ast_context, syntax):
        self.ast_context = ast_context
        self.syntax = syntax

        self.visiting_func = False
        self.visiting_attr = False

    def call(self, node, num_children_visited):
        if num_children_visited == 0:
            assert self.visiting_func == False
            self.visiting_func = True
        elif num_children_visited == 1:
            assert self.visiting_func == True
            self.visiting_func = False

    def attr(self, node, num_children_visited):
        assert self.visiting_func
        if num_children_visited == 0:
            assert not self.visiting_attr
            self.visiting_attr = True
        if num_children_visited == -1:
            assert self.visiting_attr
            self.visiting_attr = False


class FuncCallVisitor(_CommonStateVisitor):
    """
    Calls rewrite rules on the AST.
    """

    def __init__(self, ast_context, syntax):
        super().__init__(ast_context, syntax)

        # needs to be a stack for nested func names, for example:
        # print("foo".startswith("f"))
        # stack stores tuples: (func name, target type)
        # for example: ("print", None) or ("append", list)
        self.func_name_stack = []

        # current target type
        self.target_type=None

        # used to determine whether any nodes have actually been rewritten
        self.rewritten_nodes = []

    def assign(self, node, num_children_visited):
        if num_children_visited == -1:
            assert len(node.targets) == 1
            # use '=' to transform into a function call
            self._handle_function_call("<>_=", None, node, arg_nodes=[node.targets[0], node.value])

    def attr(self, node, num_children_visited):
        super().attr(node, num_children_visited)
        if num_children_visited == 0:
            assert self.target_type is None
        if num_children_visited == -1:
            self.func_name_stack.append((node.attr, self.target_type))
            self.target_type = None

    def binop(self, node, num_children_visited):
        if num_children_visited == -1:
            if isinstance(node.op, ast.Add):
                op = "+"
            elif isinstance(node.op, ast.Mult):
                op = "*"
            else:
                assert False, "Unhandled binop"
            self._handle_function_call("<>_%s" % op, None, node, [node.left, node.right])

    def compare(self, node, num_children_visited):
        if num_children_visited == -1:
            assert len(node.ops) == 1
            assert len(node.comparators) == 1
            if isinstance(node.ops[0], ast.Eq):
                op = "<>_=="
            else:
                assert False, "Unhandled comparison %s" % node.ops[0]
            self._handle_function_call(op, None, node, [node.left, node.comparators[0]])

    def call(self, node, num_children_visited):
        super().call(node, num_children_visited)
        if num_children_visited == -1:
            func_name, target_type = self.func_name_stack.pop()
            self._handle_function_call(func_name, target_type, node, node.args)

    def cond_if(self, node, num_children_visited):
        if num_children_visited == -1:
            # we'll pretend this is a function call so we have a rewrite hook
            self._handle_function_call("<>_if", None, node, arg_nodes=[node.test])

    def lst(self, node, num_children_visited):
        if num_children_visited == -1:
            # we'll pretend this is a function call so we have a rewrite hook
            self._handle_function_call("<>_new_list", list, node, arg_nodes=node.elts)

    def name(self, node, num_children_visited):
        if self.visiting_func:
            if self.visiting_attr:
                ti = self.ast_context.lookup_type_info_by_node(node)
                assert ti is not None
                # seems like this won't work well with nesting, we need another
                # stack?
                self.target_type = ti.value_type
            else:
                self.func_name_stack.append((node.id, None))

    def constant(self, node, num_children_visited):
        if self.visiting_attr:
            self.target_type = type(node.value)

    def _handle_function_call(self, func_name, target_type, node, arg_nodes):
        if hasattr(node, nodeattrs.REWRITTEN_NODE_ATTR):
            return
        if func_name in self.syntax.functions:
            func = self.syntax.functions[func_name]
            # make sure the target types match so that if rewriting
            # list.append is requested we don't rewrite custom_type.append
            if target_type == func.py_type:
                args = []
                for arg_node in arg_nodes:
                    type_info = self.ast_context.lookup_type_info_by_node(arg_node)
                    assert type_info is not None, "unable to lookup type info for function %s: arg %s" % (func_name, arg_node)
                    args.append(syntax.Argument(arg_node, type_info.value_type))
                rw = astrewriter.ASTRewriter(node, arg_nodes, self.ast_context)
                # the actual AST rewriting happens here:
                if func.target_name is not None:
                    rw.rename(func.target_name)
                if func.function_rewrite is not None:
                    func.function_rewrite(args, rw)
                self.rewritten_nodes.append(node)
                setattr(node, nodeattrs.REWRITTEN_NODE_ATTR, True)


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
                assert type_info is not None, "Unable to lookup type of assignment rhs %s" % node.value
                # TODO copy type_info first?
                self.id_name_to_type_info[self.lhs_value] = type_info
                assert len(node.targets) == 1
                self.ast_context.register_type_info_by_node(node.targets[0], type_info)

    def attr(self, node, num_children_visited):
        super().attr(node, num_children_visited)
        if num_children_visited == -1:
            func_name = node.attr
            assert func_name in pybuiltins.BUILT_IN_FUNCS, "Unknown attr %s" % func_name
            t = pybuiltins.BUILT_IN_FUNCS[func_name]
            self.ast_context.register_type_info_by_node(node, context.TypeInfo(t))

    def binop(self, node, num_children_visited):
        if num_children_visited == -1:
            self._register_node_target_type(node, node.left, node.right)

    def call(self, node, num_children_visited):
        super().call(node, num_children_visited)
        if num_children_visited == -1:
            rtn_type_info = self.ast_context.lookup_type_info_by_node(node.func)
            assert rtn_type_info is not None, "no rtn type for %s" % node.func
            self.ast_context.register_type_info_by_node(node, rtn_type_info)

    def lst(self, node, num_children_visited):
        if num_children_visited == -1:
            type_info = self._register_literal_type(node, node.elts)
            for el in node.elts:
                t = self.ast_context.lookup_type_info_by_node(el).value_type
                assert t is not None
                type_info.register_contained_type(t)

    def compare(self, node, num_children_visited):
        if num_children_visited == -1:
            assert len(node.comparators) == 1
            self._register_literal_type(node, True) # register boolean type

    def name(self, node, num_children_visited):
        if self.visiting_attr:
            # for example n.startswith or n.append: associate the right type
            # with node 'n'
            type_info = self.id_name_to_type_info.get(node.id, None)
            assert type_info is not None, "cannot lookup type info by id name %s" % node.id
            self.ast_context.register_type_info_by_node(node, type_info)
        elif self.visiting_func:
            func_name = node.id
            assert func_name in pybuiltins.BUILT_IN_FUNCS, "Unknown function %s" % func_name
            t = pybuiltins.BUILT_IN_FUNCS[func_name]
            self.ast_context.register_type_info_by_node(node, context.TypeInfo(t))
        elif self.visiting_lhs:
            self.lhs_value = node.id
        else:
            # a = b or print(b) or any other b ref - lookup b's type
            type_info = self.id_name_to_type_info.get(node.id, None)
            assert type_info is not None, "Cannot find type info for '%s'" % node.id
            self.ast_context.register_type_info_by_node(node, type_info)

    def num(self, node, num_children_visited):
        self._register_literal_type(node, node.n)

    def string(self, node, num_children_visited):
        self._register_literal_type(node, node.s)

    def constant(self, node, num_children_visited):
        self._register_literal_type(node, node.value)

    def _register_literal_type(self, node, value):
        type_info = context.TypeInfo(type(value))
        self.ast_context.register_type_info_by_node(node, type_info)
        return type_info

    def _register_node_target_type(self, target_node, lhs_node, rhs_node):
        lhs_type_info = self.ast_context.lookup_type_info_by_node(lhs_node)
        assert lhs_type_info is not None, "Unable to lookup LHS node type"
        rhs_type_info = self.ast_context.lookup_type_info_by_node(rhs_node)
        assert rhs_type_info is not None, "Unable to find RHS node type"
        target_type = self.syntax.combine_types(lhs_type_info.value_type,
                                                rhs_type_info.value_type)
        target_type_info = context.TypeInfo(target_type)
        self.ast_context.register_type_info_by_node(target_node, target_type_info)


class ContainerTypeVisitor(visitor.NoopNodeVisitor):
    """
    For container types (list ...), determines the type of
    the elements being added.
    """

    def __init__(self, ast_context):
        self.ctx = ast_context
        self.visiting_func = False
        self.target_node = None

    def call(self, node, num_children_visited):
        """
        Looks for list.append calls.
        """
        if num_children_visited == 0:
            if isinstance(node.func, ast.Attribute):
                # this is a method call (ie target_inst.method call)
                if isinstance(node.func.value, ast.Name):
                    # ensure we are derefercing an identifier
                    if node.func.attr == "append":
                        n = node.func.value
                        ti = self.ctx.lookup_type_info_by_node(n)
                        assert ti is not None, "cannot lookup type info for %s" % n
                        if ti.value_type is list:
                            # we only care about append calls on list types
                            for arg in node.args:
                                ati = self.ctx.lookup_type_info_by_node(arg)
                                assert ati is not None, "cannot lookup type info for arg node %s" % arg
                                ti.register_contained_type(ati.value_type)


class NodeDebugVisitor(visitor.NoopNodeVisitor):

    def __init__(self):
        self.indent = 0

    def print_node(self, node, num_children_visited):
        print("%s%s %s" % (" "*self.indent, node, num_children_visited))
        if num_children_visited == 0:
            self.indent += 1
        elif num_children_visited == -1:
            self.indent -= 1

    def add(self, node, num_children_visited):
        self.print_node(node, num_children_visited)

    def attr(self, node, num_children_visited):
        self.print_node(node, num_children_visited)
        if num_children_visited == -1:
            print(" "*self.indent, "Attribute:", node.attr)

    def binop(self, node, num_children_visited):
        self.print_node(node, num_children_visited)

    def assign(self, node, num_children_visited):
        print(node, num_children_visited)

    def call(self, node, num_children_visited):
        self.print_node(node, num_children_visited)

    def compare(self, node, num_children_visited):
        self.print_node(node, num_children_visited)

    def cond_if(self, node, num_children_visited):
        self.print_node(node, num_children_visited)

    def cond_else(self, node, num_children_visited):
        self.print_node(node, num_children_visited)

    def constant(self, node, num_children_visited):
        self.print_node(node, num_children_visited)
        print(" "*self.indent, "Constant:", node.value)

    def eq(self, node, num_children_visited):
        self.print_node(node, num_children_visited)

    def expr(self, node, num_children_visited):
        self.print_node(node, num_children_visited)

    def funcdef(self, node, num_children_visited):
        self.print_node(node, num_children_visited)

    def module(self, node, num_children_visited):
        self.print_node(node, num_children_visited)

    def mult(self, node, num_children_visited):
        self.print_node(node, num_children_visited)

    def name(self, node, num_children_visited):
        self.print_node(node, num_children_visited)
        print(" "*self.indent, "Name:", node.id)

    def name_constant(self, node, num_children_visited):
        self.print_node(node, num_children_visited)

    def num(self, node, num_children_visited):
        self.print_node(node, num_children_visited)

    def rtn(self, node, num_children_visited):
        self.print_node(node, num_children_visited)

    def string(self, node, num_children_visited):
        self.print_node(node, num_children_visited)
