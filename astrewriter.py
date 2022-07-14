import ast
import nodebuilder
import context
import copy
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
        n = nodebuilder.call(function_name)
        return ASTRewriter(n, arg_nodes=[], ast_context=self.ast_context)

    def const(self, value):
        """
        Returns a wrapped ast.Constant node.
        """
        n = nodebuilder.constant(value)
        return ASTRewriter(n, arg_nodes=[], ast_context=self.ast_context)

    def ident(self, name):
        """
        Returns a wrapped ast.Name (identifier) node.
        """
        n = nodebuilder.identifier(name)
        return ASTRewriter(n, arg_nodes=[], ast_context=self.ast_context)

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

    def reassign_to_arg(self):
        """
        sorted(a) -> a = sorted(a)

        This is the more complicated example for which we want this,
        which requires 2 rewrites:

        Python:
          l = [1]
          l.append(2)

        Elisp:
          (setq l (list 1))
          (append l 2) # 1st rewrite (not done by this method)
          (setq l (append l 2)) # 2nd rewrite (this method)
        """
        call_node = getattr(self.node, nodeattrs.ALT_NODE_ATTR, self.node)
        assert isinstance(call_node, ast.Call)
        first_arg_node = self.arg_nodes[0]
        first_arg_node_type_info = self.ast_context.lookup_type_info_by_node(first_arg_node)
        assert first_arg_node_type_info is not None
        assign_lhs_node = nodebuilder.identifier(first_arg_node.id)
        assign_node = ast.Assign()
        assign_node.targets = [assign_lhs_node]

        # we need to shallow copy the node so that when we set the ALT_NODE_ATTR
        # on self.node, it is really only set on self.node.
        # otherwise we get:
        # this node (n1) 's alt node -> assign_node (n2) -> assign_node.value (n1)
        # -> circular reference n1 -> n2 -> n1
        assign_node.value = copy.copy(call_node)
        # required so that this call node does not get rewritten again
        setattr(assign_node.value, nodeattrs.REWRITTEN_NODE_ATTR, True)
        assert not hasattr(call_node, nodeattrs.ALT_NODE_ATTR)
        self.ast_context.register_type_info_by_node(assign_lhs_node, first_arg_node_type_info)
        self.ast_context.register_type_info_by_node(assign_node.value, first_arg_node_type_info)
        setattr(self.node, nodeattrs.ALT_NODE_ATTR, assign_node)

    def rewrite_as_func_call(self, inst_1st=False, inst_renamer=None):
        """
        Rewrites <instance>.<method>(args) as <method>(args + [<instance>]).

        This is the opposite of rewrite_as_attr_method_call.

        Call
          Attr
            Const|Name: instance call is made on
            attr: attribute (method) name
          Args ...

        needs to be rewritten to:

        Call
          Name: function name
          Args ... insert Call.Attr.attr here

        inst_1st: if True, the target instance becomes the first argument,
        in the function call, if False, it will be the last argument.

        inst_renamer: a function that, if not None, given the name of the inst
        arg, will provide an alternative name.
        """
        node = getattr(self.node, nodeattrs.ALT_NODE_ATTR, self.node)
        assert isinstance(node, ast.Call)
        assert isinstance(node.func, ast.Attribute)
        inst_arg_node = node.func.value
        if inst_renamer is not None:
            assert isinstance(node.func.value, ast.Name)
            new_name = inst_renamer(node.func.value.id)
            inst_arg_node = nodebuilder.identifier(new_name)
        if inst_1st:
            self.prepend_arg(inst_arg_node)
        else:
            self.append_arg(inst_arg_node)
        # remove the attribute ref, keep the ref (function) name
        func_name = ast.Name()
        func_name.id = node.func.attr
        node.func = func_name
        return self

    def rewrite_as_attr_method_call(self):
        """
        Rewrites <method>(args + [<instance>]) as <instance>.<method>(args).

        This is the opposite of rewrite_as_func_call.
        """
        node = getattr(self.node, nodeattrs.ALT_NODE_ATTR, self.node)
        assert isinstance(node, ast.Call)
        assert isinstance(node.func, ast.Name)
        assert len(self.arg_nodes) >= 0
        attr_node = ast.Attribute()
        # by default the first arg becomes the target instance
        # this is necessary for the case str1 == str2, which should be converted
        # to str1.equals(str2)
        attr_node.value = node.args[0]
        attr_node.attr = node.func.id
        del node.args[0]
        node.func = attr_node
        return self

    def chain_method_call(self, method_name, args=[]):
        assert isinstance(self.node, ast.Call)
        node = getattr(self.node, nodeattrs.ALT_NODE_ATTR, self.node)        
        org_call = copy.copy(node) # shallow copy - see other place
        setattr(org_call, nodeattrs.REWRITTEN_NODE_ATTR, True)
        attr_node = ast.Attribute()
        setattr(attr_node, nodeattrs.REWRITTEN_NODE_ATTR, True)
        attr_node.value = org_call
        attr_node.attr = method_name
        new_call = nodebuilder.call(attr_node, args, [nodeattrs.REWRITTEN_NODE_ATTR])
        setattr(self.node, nodeattrs.ALT_NODE_ATTR, new_call)
        setattr(self.node, nodeattrs.REWRITTEN_NODE_ATTR, True)        
        return self

    def replace_node_with(self, rewriter, keep_args=True):
        assert isinstance(rewriter, ASTRewriter),\
            "replace_node_with must be called with an ASTRewriter instance"
        target_node = rewriter.node
        assert not hasattr(self.node, nodeattrs.ALT_NODE_ATTR)
        setattr(self.node, nodeattrs.ALT_NODE_ATTR, target_node)
        self._copy_special_node_attrs(self.node, target_node)
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
