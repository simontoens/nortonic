import ast
import context
import nodeattrs


class ASTTransformer:
    """
    Fluid builder-type API for simple AST operations.

    For example:
        print(1 , 2) -> System.out.println(String.format("%d %d", 1, 2))
    """
    def __init__(self, node, arg_nodes, child_nodes, ast_context):
        self.node = node
        self.arg_nodes = arg_nodes # print(1, 2): [1, 2]
        #self.child_nodes = child_nodes # if -> body
        self.ast_context = ast_context

        self.appended_args = []
        self.prepended_args = []
        #self.prepended_body_node = None

    def wrap(self, node):
        return ASTTransformer(node, arg_nodes=[], child_nodes=[], ast_context=self.ast_context)        

    # @property
    # def body_nodes(self):
    #     return self.child_nodes

    def call(self, function_name):
        call_node = ast.Call()
        call_node.func = ast.Name()
        call_node.func.id = function_name
        call_node.args = []
        call_node.keywords = []
        return ASTTransformer(call_node, arg_nodes=[], child_nodes=[], ast_context=self.ast_context)

    def rename(self, name):
        self.node.func.id = name
        return self

    def replace_node_with(self, transformer, keep_args=True):
        assert isinstance(transformer, ASTTransformer),\
            "replace_node_with must be called with a ASTTransformer instance"
        target_node = transformer.node
        assert not hasattr(self.node, nodeattrs.ALT_NODE_ATTR)
        setattr(self.node, nodeattrs.ALT_NODE_ATTR, target_node)
        self._copy_special_node_attrs(self.node, target_node)
        assert isinstance(target_node, ast.Call),\
            "replace_node_with must be putting a call node in place but got %s" % target_node
        if keep_args:
            target_node.args = []
            target_node.args += transformer.prepended_args
            target_node.args += self.arg_nodes
            target_node.args += transformer.appended_args
        # if transformer.prepended_body_node is not None:
        #     transformer.prepended_body_node.args += self.child_nodes
        #     target_node.args.append(transformer.prepended_body_node)
        # else:
        #     target_node.args += self.child_nodes
        return self

    def replace_args_with(self, value):
        self.node.args = []
        self.append_arg(value)

    # def insert_body_node(self, transformer):
    #     assert isinstance(transformer, ASTTransformer),\
    #         "prepend_body_node must be called with a ASTTransformer instance"
    #     assert self.prepended_body_node is None
    #     self.prepended_body_node = transformer.node
    #     return self

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

    def indent_incr(self):
        setattr(self.node, nodeattrs.INDENT_INCR_NODE_ATTR, True)
        return self

    def indent_decr(self):
        setattr(self.node, nodeattrs.INDENT_DECR_NODE_ATTR, True)
        return self

    def newline(self):
        setattr(self.node, nodeattrs.NEWLINE_NODE_ATTR, True)
        return self

    def block(self):
        """
        Marks this node as starting a block.
        """
        setattr(self.node, nodeattrs.BLOCK_START_NODE_ATTR, True)
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
            if hasattr(arg_node, nodeattrs.ALT_NODE_ATTR):
                alt_node = getattr(arg_node, nodeattrs.ALT_NODE_ATTR)
                self._copy_special_node_attrs(arg_node, alt_node)
            if append:
                self.node.args.append(arg_node)
                self.appended_args.append(arg_node)
            else:
                self.node.args.insert(0, arg_node)
                self.prepended_args.append(arg_node)
        return self

    def _copy_special_node_attrs(self, src_node, target_node):
        for attr in nodeattrs.ALL_SETTABLE_ATTRS:
            if hasattr(src_node, attr):
                setattr(target_node, attr, True)
        
