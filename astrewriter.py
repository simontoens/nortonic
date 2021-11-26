import ast
import context
import nodeattrs


class ASTRewriter:
    """
    Convenience methods for common AST rewrites.  Methods return self to
    allow chaining in lambda expressions.

    For example, to rewrite this function call: print(1 , 2)
    to: System.out.println(String.format("%d %d", 1, 2))

    the code looks something like (with minor omissions):

    rewrite=lambda args, rw:
        rw.replace_args_with(
            rw.call("String.format")
                .prepend_arg(" ".join([self._fmt[a.type] for a in args]))
                .append_args([a.node for a in args]))
        if len(args) > 1 else None)
    """
    def __init__(self, node, arg_nodes, ast_context):
        self.node = node
        self.arg_nodes = arg_nodes # print(1, 2): [1, 2]
        self.ast_context = ast_context

        self.appended_args = []
        self.prepended_args = []

    def wrap(self, node):
        return ASTRewriter(node, arg_nodes=[], ast_context=self.ast_context)

    def call(self, function_name):
        """
        Returns a wrapped ast.Call (function invocation) node.
        """
        call_node = ast.Call()
        call_node.func = ast.Name()
        call_node.func.id = function_name
        call_node.args = []
        call_node.keywords = []
        return ASTRewriter(call_node, arg_nodes=[], ast_context=self.ast_context)

    def rename(self, name):
        """
        Renames the wrapped function represented by the wrapped Call node to
        the specified name. The call node may have a child Name node (f("foo"))
        or a child Attr node (thing.f("foo")).
        """
        assert isinstance(self.node, ast.Call)
        if isinstance(self.node.func, ast.Attribute):
            self.node.func.attr = name
        else:
            self.node.func.id = name
        return self

    def replace_with_func_call(self):
        """
        Rewrites <instance>.<method>(args) as <method>(args + [<instance>]).

        Call
          Attr
            Const|Name: instance call is made on
            attr: attribute (method) name
          Args ...

        needs to be rewritten to:

        Call
          Name: function name
          Args ... insert Call.Attr.attr here
        """
        assert isinstance(self.node, ast.Call)
        assert isinstance(self.node.func, ast.Attribute)
        # default behavior is to append the target instance as an arg
        self.append_arg(self.node.func.value)
        # remove the attribute ref, keep the ref (function) name
        func_name = ast.Name()
        func_name.id = self.node.func.attr
        self.node.func = func_name
        return self

    def replace_node_with(self, rewriter, keep_args=True):
        assert isinstance(rewriter, ASTRewriter),\
            "replace_node_with must be called with an ASTRewriter instance"
        target_node = rewriter.node
        assert not hasattr(self.node, nodeattrs.ALT_NODE_ATTR)
        setattr(self.node, nodeattrs.ALT_NODE_ATTR, target_node)
        self._copy_special_node_attrs(self.node, target_node)
        assert isinstance(target_node, ast.Call),\
            "replace_node_with must be putting a call node in place but got %s" % target_node
        if keep_args:
            target_node.args = []
            target_node.args += rewriter.prepended_args
            target_node.args += self.arg_nodes
            target_node.args += rewriter.appended_args
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

    def indent_incr(self):
        setattr(self.node, nodeattrs.INDENT_INCR_NODE_ATTR, True)
        return self

    def indent_decr(self):
        setattr(self.node, nodeattrs.INDENT_DECR_NODE_ATTR, True)
        return self

    def indent_around(self):
        setattr(self.node, nodeattrs.INDENT_AROUND_NODE_ATTR, True)
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
            if isinstance(arg, ASTRewriter):
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

