from target import targetlanguage
import ast
import context
import copy
import nodeattrs
import nodebuilder


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
    def __init__(self, node, arg_nodes, ast_context, parent_body, target_node=None):
        self.node = node
        self._arg_nodes = list(arg_nodes) # print(1, 2): [1, 2]
        self.ast_context = ast_context
        self.parent_body = parent_body
        self.target_node = target_node

        self._appended_args = []
        self._prepended_args = []

    @property
    def arg_nodes(self):
        args = []
        args += self._prepended_args
        args += self._arg_nodes
        args += self._appended_args
        return args

    def append_to_body(self, *nodes):
        if not hasattr(self.node, "body"):
            self.node.body = []
        assert len(nodes) > 0
        nodes_to_add = nodes
        if len(nodes) == 1 and isinstance(nodes[0], (list, tuple)):
            nodes_to_add = nodes[0]
        self.node.body += [self._to_ast_node(n) for n in nodes_to_add]
        return self

    def wrap(self, node):
        return ASTRewriter(node, arg_nodes=[], ast_context=self.ast_context,
                           parent_body=self.parent_body)

    def call(self, function_name):
        """
        Returns a wrapped ast.Call (function invocation) node.
        """
        n = nodebuilder.call(function_name)
        return ASTRewriter(n, arg_nodes=[], ast_context=self.ast_context,
                           parent_body=self.parent_body)

    def const(self, value):
        """
        Returns a wrapped ast.Constant node.
        """
        n = nodebuilder.constant(value)
        return ASTRewriter(n, arg_nodes=[], ast_context=self.ast_context,
                           parent_body=self.parent_body)

    def ident(self, name):
        """
        Returns a wrapped ast.Name (identifier) node.
        """
        n = nodebuilder.identifier(name)
        return ASTRewriter(n, arg_nodes=[], ast_context=self.ast_context,
                           parent_body=self.parent_body)

    def binop(self, op, lhs, rhs):
        """
        Returns a wrapped ast.BinOp node.
        """
        n = nodebuilder.binop(op, lhs, rhs)
        assert isinstance(lhs, ast.AST)
        lhs_type_info = self.ast_context.get_type_info_by_node(lhs)
        self.ast_context.register_type_info_by_node(n, lhs_type_info)
        assert not isinstance(rhs, ast.AST)
        rhs_type_info = context.TypeInfo.int()
        self.ast_context.register_type_info_by_node(n.right, rhs_type_info)
        return ASTRewriter(n, arg_nodes=[], ast_context=self.ast_context,
                           parent_body=self.parent_body)

    def rename(self, name):
        """
        Renames the function or attribute represented by the wrapped node
        to the specified name.
        """
        if isinstance(self.node, ast.Call):
            if isinstance(self.node.func, ast.Attribute):
                self.node.func.attr = name
            else:
                self.node.func.id = name
        elif isinstance(self.node, ast.Attribute):
            self.node.attr = name
        else:
            assert False, "bad node type %s" % self.node
        return self

    def reassign_to_arg(self):
        """
        sorted(a) -> a = sorted(a)

        This is the more complicated example for which we want this,
        which requires 2 rewrites:

        Python:
            l = []
            l.append(2)

        Golang:
            l := []string{}
            l = append(l, "hello")

        self.register_function_rewrite(
            py_name="append", py_type=list,
            rewrite=lambda args, rw: rw
                .rewrite_as_func_call(inst_1st=True).reassign_to_arg())
        """
        call_node = getattr(self.node, nodeattrs.ALT_NODE_ATTR, self.node)
        assert isinstance(call_node, ast.Call)
        assert len(self.arg_nodes) > 0
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
        self.ast_context.register_type_info_by_node(assign_node, first_arg_node_type_info)
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
        assert isinstance(node, ast.Call), "expected Call node but got %s" % node
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

    def call_on_target(self, method_name, keep_args=True):
        """
        Calls the specific method on the contextual target (instance).

        This replaces the existing node.

        For example:
            l = [1]
            i = l[0] <- l is the target, this method can rewrite as l.get(0)
        """
        assert self.target_node is not None
        args = self.arg_nodes if keep_args else []
        attr_call_node = nodebuilder.attr_call(self.target_node, method_name, args)
        setattr(self.node, nodeattrs.ALT_NODE_ATTR, attr_call_node)
        return self

    def call_with_target_as_arg(self, func_name, target_as_first_arg=True):
        """
        Calls the specific function and and makes the contextual target
        (instance) either the first or the last argument.

        This replaces the existing node.

        For example:
            l = [1]
            i = l[0] <- l is the target, this method can rewrite as (nth 0 l)
        """
        assert self.target_node is not None
        arg_nodes = [self.target_node] + self.arg_nodes if target_as_first_arg else self.arg_nodes + [self.target_node]
        call_node = nodebuilder.call(func_name, arg_nodes)
        setattr(self.node, nodeattrs.ALT_NODE_ATTR, call_node)
        return self

    def chain_method_call(self, method_name, args=[]):
        assert isinstance(self.node, (ast.Call, ast.Name))
        node = getattr(self.node, nodeattrs.ALT_NODE_ATTR, self.node)
        node_type_info = self.ast_context.lookup_type_info_by_node(node)
        org_call = copy.copy(node) # shallow copy - see other place
        setattr(org_call, nodeattrs.REWRITTEN_NODE_ATTR, True)
        self.ast_context.register_type_info_by_node(org_call, node_type_info)

        attr_node = ast.Attribute()
        setattr(attr_node, nodeattrs.REWRITTEN_NODE_ATTR, True)
        attr_node.value = org_call
        attr_node.attr = method_name
        new_call = nodebuilder.call(attr_node, args, [nodeattrs.REWRITTEN_NODE_ATTR])
        self.ast_context.register_type_info_by_node(new_call, node_type_info)
        setattr(self.node, nodeattrs.ALT_NODE_ATTR, new_call)
        setattr(self.node, nodeattrs.REWRITTEN_NODE_ATTR, True)        
        return self

    def replace_node_with(self, rewriter, keep_args=True,
                          current_node_becomes_singleton_arg=False):
        assert isinstance(rewriter, ASTRewriter),\
            "replace_node_with must be called with an ASTRewriter instance"
        current_node = getattr(self.node, nodeattrs.ALT_NODE_ATTR, self.node)
        target_node = rewriter.node
        type_info = self.ast_context.lookup_type_info_by_node(current_node)
        self.ast_context.register_type_info_by_node(target_node, type_info)
        self._copy_special_node_attrs(current_node, target_node)
        if current_node_becomes_singleton_arg:
            keep_args = False
            target_node.args = []
            arg_node = copy.copy(current_node)
            setattr(arg_node, nodeattrs.REWRITTEN_NODE_ATTR, True)
            target_node.args.append(arg_node)
        if keep_args:
            target_node.args = []
            target_node.args += rewriter._prepended_args
            target_node.args += self._arg_nodes
            target_node.args += rewriter._appended_args
        setattr(self.node, nodeattrs.ALT_NODE_ATTR, target_node)
        return self

    def _get_for_loop_range_nodes(self):
        assert isinstance(self.node, ast.For)
        rhs = self.node.iter.get()
        if isinstance(rhs, ast.Call) and rhs.func.id == "range":
            start_node = rhs.args[0].get()
            end_node = rhs.args[1].get()
            if len(rhs.args) == 2:
                step_node = nodebuilder.constant(1)
            else:
                step_node = rhs.args[2].get()
            return (start_node, end_node, step_node)
        return None

    def _get_for_loop_enumerated_iter_node(self):
        assert isinstance(self.node, ast.For)
        rhs = self.node.iter.get()
        if isinstance(rhs, ast.Call) and rhs.func.id == "enumerate":
            return rhs.args[0].get()
        return None

    def is_range_loop(self):
        return self._get_for_loop_range_nodes() is not None

    def is_enumerated_loop(self):
        return self._get_for_loop_enumerated_iter_node() is not None

    def rewrite_as_c_style_loop(self):
        assert isinstance(self.node, ast.For)
        for_loop_range_nodes = self._get_for_loop_range_nodes()
        if for_loop_range_nodes is None:
            enumerated_iter_node = self._get_for_loop_enumerated_iter_node()
            target_node = self.node.target.get() # the iterating var
            iter_node = self.node.iter.get() # what we're iterating over
            counter_var_name = None
            if enumerated_iter_node is None:
                # for l in my_list:
                counter_var_name = self.ast_context.get_unqiue_identifier_name("i")
            else:
                # for i, l in enumerate(my_list):
                assert isinstance(target_node, ast.Tuple), "got node type %s" % iter_node
                counter_var_name = target_node.elts[0].get().id # i
                target_node = target_node.elts[1].get()
                iter_node = enumerated_iter_node # my_list
                assert isinstance(iter_node, ast.Name)
            self._rewrite_as_c_style_loop__foreach(target_node, iter_node, counter_var_name)
        else:
            self._rewrite_as_c_style_loop__range(for_loop_range_nodes)
        return self

    def get_for_loop_init_node(self):
        return getattr(self.node, nodeattrs.FOR_LOOP_C_STYLE_INIT_NODE)

    def get_for_loop_cond_node(self):
        return getattr(self.node, nodeattrs.FOR_LOOP_C_STYLE_COND_NODE)

    def get_for_loop_expr_node(self):
        return getattr(self.node, nodeattrs.FOR_LOOP_C_STYLE_EXPR_NODE)

    def _rewrite_as_c_style_loop__foreach(self, target_node, iter_node, counter_var_name):
        """
        Rewrites a "foreach" style loop into a c-style loop with counter
        variable.

        target_node - the ast.Name node that each iterated element is assigned
        iter_node - the iterable
        counter_var_name - the var name to use as counter ("i")
        """
        int_ti = context.TypeInfo.int()
        init_node = nodebuilder.assignment(counter_var_name, 0)
        self.ast_context.register_type_info_by_node(init_node.value, int_ti)
        self.ast_context.register_type_info_by_node(init_node.targets[0], int_ti)
        end_node = nodebuilder.call("len", [iter_node])
        self.ast_context.register_type_info_by_node(end_node, int_ti)
        setattr(self.node, nodeattrs.FOR_LOOP_C_STYLE_INIT_NODE, init_node)
        setattr(self.node, nodeattrs.FOR_LOOP_C_STYLE_COND_NODE,
                nodebuilder.condition(counter_var_name, "<", end_node))
        setattr(self.node, nodeattrs.FOR_LOOP_C_STYLE_EXPR_NODE,
                nodebuilder.reassignment(counter_var_name, 1, "+"))

        sub_node = nodebuilder.subscript_list(iter_node, counter_var_name)
        self.ast_context.register_type_info_by_node(sub_node.slice, int_ti)
        target_ass_node = nodebuilder.assignment(target_node, sub_node)
        target_type_info = self.ast_context.get_type_info_by_node(target_node)
        self.ast_context.register_type_info_by_node(target_ass_node, target_type_info)
        self.ast_context.register_type_info_by_node(target_ass_node.value, target_type_info)
        self.node.body.insert(0, target_ass_node)

    def _rewrite_as_c_style_loop__range(self, for_loop_range_nodes):
        start_node, end_node, step_node = for_loop_range_nodes
        target_node = self.node.target.get()
        assert isinstance(target_node, ast.Name)
        target_node_name = target_node.id
        target_type_info = self.ast_context.get_type_info_by_node(target_node)
        init_node = nodebuilder.assignment(target_node_name, start_node)
        self.ast_context.register_type_info_by_node(init_node.targets[0], target_type_info)
        self.ast_context.register_type_info_by_node(init_node, target_type_info)
        setattr(self.node, nodeattrs.FOR_LOOP_C_STYLE_INIT_NODE, init_node)
        # figure out the comparsion op, but this is impossible if
        # start_value/end_value/step are not constants
        op = "<"
        if isinstance(step_node, ast.UnaryOp):
            if isinstance(step_node.op, ast.USub):
                op = ">"
        setattr(self.node, nodeattrs.FOR_LOOP_C_STYLE_COND_NODE,
                nodebuilder.condition(target_node_name, op, end_node))
        setattr(self.node, nodeattrs.FOR_LOOP_C_STYLE_EXPR_NODE,
                nodebuilder.reassignment(target_node_name, step_node, "+"))

    def rewrite_as_if_stmt(self):
        """
        This method rewrites if-expression usage in specific contexts as
        regular if-statements.
        It is non-trivial to write a general solution to this translation, so
        they have to be handled case-by-case.

        1) assignment:
            a = 3 if 0 == 0 else 2
            ->
            if 0 == 0:
                a = 3
            else:
                a = 2

        2) return:
            return 1 if 0 == 0 else 2
            ->
            if 0 == 0:
                return 1
            else:
                return 2
        """
        assert isinstance(self.node, ast.IfExp)
        arg_nodes = self.arg_nodes
        # given this if-expr:
        # a = 3 if 0 == 0 else 2
        # body: 3 <Constant Node>
        # test: 0 == 0 <Compare Node>
        # orelse: 2 <Constant Node>
        # if_expr_parent_node: a = <IfExp Node>
        org_body = arg_nodes[0]
        org_test = arg_nodes[1]
        org_orelse = arg_nodes[2]
        if_expr_parent_node = arg_nodes[3]
        if isinstance(if_expr_parent_node, ast.Assign):
            org_assign_lhs = if_expr_parent_node.targets[0]
            body_node = nodebuilder.assignment(
                copy.copy(org_assign_lhs), org_body)
            orelse_node = nodebuilder.assignment(
                copy.copy(org_assign_lhs), org_orelse)
        elif isinstance(if_expr_parent_node, ast.Return):
            body_node = copy.copy(if_expr_parent_node)
            body_node.value = org_body
            orelse_node = copy.copy(if_expr_parent_node)
            orelse_node.value = org_orelse
        else:
            raise AssertionError("Unhandled if-expr rewrite %s" % if_expr_parent_node)
        if_node = nodebuilder.if_stmt(
            body=body_node, test=org_test, orelse=orelse_node)
        setattr(if_expr_parent_node, nodeattrs.ALT_NODE_ATTR, if_node)

    def insert_above(self, rewriter):
        assert isinstance(rewriter, ASTRewriter)
        nodebuilder.insert_node_above(rewriter.node, self.parent_body, self.node)
        return self

    def remove_args(self):
        self.node.args = []
        return self

    def replace_args_with(self, value):
        self.node.args = []
        self.append_arg(value)
        return self

    def prepend_arg(self, *args):
        return self._add_arg(append=False, args=args)

    def append_arg(self, arg):
        return self._add_arg(append=True, args=[arg])

    def append_args(self, args):
        assert isinstance(args, (list, tuple)), "unexpected type %s" % args
        return self._add_arg(append=True, args=args)

    def keep_first_arg(self):
        if len(self.arg_nodes) > 1:
            self.replace_args_with(self.arg_nodes[0])
        return self

    def _add_arg(self, append, args):
        for arg in args:
            if isinstance(arg, ASTRewriter):
                arg_node = arg.node
            elif isinstance(arg, ast.AST):
                arg_node = arg
            elif isinstance(arg, targetlanguage.Argument):
                arg_node = arg.node
            else:
                arg_node = ast.Constant()
                arg_node.value = arg
                type_info = context.TypeInfo(type(arg))
                self.ast_context.register_type_info_by_node(arg_node, type_info)
            if hasattr(arg_node, nodeattrs.ALT_NODE_ATTR):
                alt_node = getattr(arg_node, nodeattrs.ALT_NODE_ATTR)
                self._copy_special_node_attrs(arg_node, alt_node)
            node = getattr(self.node, nodeattrs.ALT_NODE_ATTR, self.node)
            if append:
                node.args.append(arg_node)
                self._appended_args.append(arg_node)
            else:
                node.args.insert(0, arg_node)
                self._prepended_args.append(arg_node)
        return self

    def _copy_special_node_attrs(self, src_node, target_node):
        for attr in nodeattrs.ATTR_NAMES:
            val = getattr(src_node, attr, None)
            if val is not None:
                setattr(target_node, attr, val)

    def _to_ast_node(self, n):
        if isinstance(n, ast.AST):
            return n
        if isinstance(n, ASTRewriter):
            return n.node
        if isinstance(n, (str, int)):
            return nodebuilder.constant(n)
        assert False, "don't know how to convert %s to an ast node" % n
