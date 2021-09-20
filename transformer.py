import ast
import context
import nodeattrs


class ASTTransformer:
    """
    Fluid builder-type API for simple AST operations.

    For example:
        print(1 , 2) -> System.out.println(String.format("%d %d", 1, 2))
    """
    def __init__(self, node, arg_nodes, ast_context):
        self.node = node
        self.arg_nodes = arg_nodes
        self.ast_context = ast_context

    def call(self, function_name):
        call_node = ast.Call()
        call_node.func = ast.Name()
        call_node.func.id = function_name
        call_node.args = []
        call_node.keywords = []
        return ASTTransformer(call_node, arg_nodes=[], ast_context=self.ast_context)

    def rename(self, name):
        self.node.func.id = name
        return self

    def replace_node_with(self, value, keep_args=True):
        assert isinstance(value, ASTTransformer),\
            "replace_node_with must be called with a ASTTransformer instance"
        setattr(self.node, nodeattrs.ALT_NODE_ATTR, value.node)
        # missing type registration for value.node?
        if keep_args:
            value.append_args(self.arg_nodes)
        return self

    def replace_args_with(self, value):
        self.node.args = []
        self.append_arg(value)

    def prepend_arg(self, *args):
        return self._add_arg(append=False, args=args)

    def append_arg(self, arg):
        return self._add_arg(append=True, args=[arg])
    
    def append_args(self, args):
        assert isinstance(args, (list, tuple))
        return self._add_arg(append=True, args=args)

    def stmt(self):
        """
        Marks this node as being a stmt.
        """
        setattr(self.node, nodeattrs.STMT_NODE_ATTR, True)
        return self

    def _add_arg(self, append, args):
        for arg in args:
            if isinstance(arg, ASTTransformer):
                arg_node = arg.node
            elif isinstance(arg, ast.AST):
                arg_node = arg
            else:
                arg_node = ast.Constant()
                arg_node.value = arg
                type_info = context.TypeInfo(type(arg))
                self.ast_context.register_type_info_by_node(arg_node, type_info)
            if append:
                self.node.args.append(arg_node)
            else:
                self.node.args.insert(0, arg_node)
        return self
