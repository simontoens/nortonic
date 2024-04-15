from target import targetlanguage
import ast
import context
import nodeattrs
import nodebuilder
import nodes


class ASTRewriter:
    """
    Convenience methods for common AST rewrites.  Methods return self to
    allow chaining.
    """
    def __init__(self, node, arg_nodes, ast_context, parent_body, target_node=None):
        self.node = node
        self._arg_nodes = list(arg_nodes) # print(1, 2): [1, 2]
        self.ast_context = ast_context
        self.parent_body = parent_body
        self._target_node = target_node

        self._appended_args = []
        self._prepended_args = []

    @property
    def target_node(self):
        return self._target_node

    @property
    def target(self):
        return self.wrap(self._target_node)
        
    @property
    def arg_nodes(self):
        args = []
        args += self._prepended_args
        args += self._arg_nodes
        args += self._appended_args
        return args

    def set_node_attr(self, attr):
        nodeattrs.set_attr(self.node, attr)
        return self

    def append_to_body(self, *nodes):
        if not hasattr(self.node, "body"):
            self.node.body = []
        assert len(nodes) > 0
        nodes_to_add = nodes
        if len(nodes) == 1 and isinstance(nodes[0], (list, tuple)):
            nodes_to_add = nodes[0]
        for n in nodes_to_add:
            n = self._get_node(n)
            ti = self.ast_context.lookup_type_info_by_node(n)
            if ti is None:
                nodeattrs.set_type_info(n, context.TypeInfo.notype())
            self.node.body.append(n)
        return self

    def wrap(self, node):
        return ASTRewriter(node, arg_nodes=[], ast_context=self.ast_context,
                           parent_body=self.parent_body)


    def call(self, function, rtn_type=None, node_metadata={}):
        """
        Returns a wrapped ast.Call (function invocation) node.

        REVIEW DOC
        If this call node will replace an existing node, the function metadata
        is derived from the node being replaced.
        # REVIEW - Document function can be a str or inst
        If this is a new, additional call node, the rtn_type must be specified
        as a context.TypeInfo instance.
        """
        assert isinstance(function, (str, context.Function, ast.Name))
        rtn_type_info = None
        if isinstance(function, str):
            function_name = function
            if rtn_type is not None:
                if isinstance(rtn_type, context.TypeInfo):
                    rtn_type_info = rtn_type
                else:
                    rtn_type_info = context.TypeInfo(rtn_type)
        elif isinstance(function, ast.Name):
            function_name = function.id
            rtn_type_info = self.ast_context.lookup_type_info_by_node(function)
        else:
            function_name = function.name
            rtn_type_info = function.get_rtn_type_info()
        call_node = nodebuilder.call(function_name)
        nodeattrs.set_node_attributes(call_node, node_metadata)
        if rtn_type_info is not None:
            # so we can find the type again when typevisitor re-runs from
            # scratch
            nodeattrs.set_type_info(call_node, rtn_type_info)
            # this is needed because subsequent ast rewrites currently assume
            # they can access the type from the context
            # ie, this call node may get rewritten, for example:
            # rw.insert_above(rw.call(context.PRINT_BUILTIN)
            self.ast_context.register_type_info_by_node(call_node, rtn_type_info)
            if nodeattrs.ASSIGN_LHS_NODE_ATTR in node_metadata:
               nodeattrs.set_type_info(node_metadata[nodeattrs.ASSIGN_LHS_NODE_ATTR], rtn_type_info)
        return ASTRewriter(call_node, arg_nodes=[],
                           ast_context=self.ast_context,
                           parent_body=self.parent_body)

    def xcall(self, function):
        """
        Dummy return type for rewrites where the return type doesn't matter,
        typically nested function calls.
        """
        return self.call(function, context.TypeInfo.notype())

    def const(self, value):
        """
        Returns a wrapped ast.Constant node.
        """
        n = nodebuilder.constant(value)
        return ASTRewriter(n, arg_nodes=[], ast_context=self.ast_context,
                           parent_body=self.parent_body)

    def ident(self, name, type_info=None, node_attrs=[]):
        """
        Returns a wrapped ast.Name (identifier) node.
        """
        return self._ident(nodebuilder.identifier(name), type_info, node_attrs)

    def xident(self, name_or_node, node_attrs=[]):
        """
        Returns a wrapped ast.Name (identifier) node.

        Returns an opaque identifier that should make sense in the context
        of the current ast rewriting.
        The special "no type" marker type is associated with this identifer.

        The given "name" may also be a ast.Name node - in that case, only the
        special "no type" type is associated with it.
        """
        if isinstance(name_or_node, str):
            return self.ident(name_or_node, context.TypeInfo.notype(), node_attrs)
        elif isinstance(name_or_node, ast.Name):
            return self._ident(name_or_node, context.TypeInfo.notype(), node_attrs)
        else:
            raise AssertionError("Unknown type: %s" % name_or_node)

    def _ident(self, name_node, type_info=None, node_attrs=[]):
        assert isinstance(name_node, ast.Name)
        if type_info is not None:
            if not isinstance(type_info, context.TypeInfo):
                type_info = context.TypeInfo(type_info)
            nodeattrs.set_type_info(name_node, type_info) # required
            # required for in-progress re-writing
            self.ast_context.register_type_info_by_node(name_node, type_info)
        nodeattrs.set_node_attributes(name_node, node_attrs)
        return ASTRewriter(name_node, arg_nodes=[],
                           ast_context=self.ast_context,
                           parent_body=self.parent_body)

    def binop(self, op, lhs, rhs, lhs_type=None, rhs_type=None):
        """
        Returns a wrapped ast.BinOp (+, -, /, ...) node.
        """
        lhs = self._get_node(lhs)
        rhs = self._get_node(rhs)
        n = nodebuilder.binop(op, lhs, rhs)
        assert isinstance(lhs, ast.AST)
        lhs_type_info = self._get_type_info(lhs, lhs_type)
        if lhs_type_info is not None:
            self.ast_context.register_type_info_by_node(n, lhs_type_info)
        rhs_type_info = self._get_type_info(rhs, rhs_type)
        if rhs_type_info is not None:
            self.ast_context.register_type_info_by_node(n.right, rhs_type_info)
        return ASTRewriter(n, arg_nodes=[], ast_context=self.ast_context,
                           parent_body=self.parent_body)

    def compare(self, op, lhs, rhs):
        """
        Returns a wrapped ast.Compare (<, >, ==, ...) node.
        """
        lhs = self._get_node(lhs)
        rhs = self._get_node(rhs)
        n = nodebuilder.compare(lhs, op, rhs)
        return ASTRewriter(n, arg_nodes=[], ast_context=self.ast_context,
                           parent_body=self.parent_body)

    def subscript_list(self, target, index):
        target = self._get_node(target)
        index = self._get_node(index)
        n = nodebuilder.subscript_list(target, index)
        return ASTRewriter(n, arg_nodes=[], ast_context=self.ast_context,
                           parent_body=self.parent_body)

    def _get_node(self, n):
        if isinstance(n, ASTRewriter):
            return n.node
        elif isinstance(n, ast.AST):
            return n
        elif isinstance(n, targetlanguage.Argument):
            return n.node
        else:
            ti = context.TypeInfo(type(n))
            n = nodebuilder.constant(n)
            self.ast_context.register_type_info_by_node(n, ti)
            return n

    def _get_type_info(self, node, type_info=None):
        if type_info is not None:
            if not isinstance(type_info, context.TypeInfo):
                type_info = context.TypeInfo(type_info)
        if type_info is None:
            type_info = self.ast_context.lookup_type_info_by_node(node)
        if type_info is None:
            type_info = nodeattrs.get_type_info(node)
        return type_info

    def less_than(self, node_attrs=[]):
        """
        Returns a wrapped ast.Lt node.
        """
        n = ast.Lt()
        nodeattrs.set_node_attributes(n, node_attrs)
        self.ast_context.register_type_info_by_node(n, context.TypeInfo.bool())
        nodeattrs.set_type_info(n, context.TypeInfo.bool())
        return ASTRewriter(n, arg_nodes=[], ast_context=self.ast_context,
                           parent_body=self.parent_body)

    def negate(self):
        """
        Negates the current node (unary not).
        """
        node = self.node.get()
        not_node = nodebuilder.unary_not(nodes.shallow_copy_node(node))
        self._set_alt_node_attr(not_node)
        return ASTRewriter(not_node, arg_nodes=[], ast_context=self.ast_context,
                           parent_body=self.parent_body)

    def funcdef_lambda(self, body, args=[]):
        assert body is not None
        body = self._get_node(body)
        args = [self._get_node(a) for a in args]
        n = nodebuilder.funcdef_lambda(body, args=args)
        return ASTRewriter(n, arg_nodes=[], ast_context=self.ast_context,
                           parent_body=self.parent_body)

    def rename(self, new_name):
        """
        Renames the function or attribute represented by the wrapped node
        to the specified name.
        """
        if isinstance(self.node, ast.Call):
            if isinstance(self.node.func, ast.Attribute):
                current_name = self.node.func.attr
                self.node.func.attr = new_name
            else:
                current_name = self.node.func.id
                self.node.func.id = new_name
        elif isinstance(self.node, ast.Attribute):
            current_name = self.node.attr
            self.node.attr = name
        else:
            assert False, "bad node type %s" % self.node

        rtn_ti = self.ast_context.get_type_info_by_node(self.node)
        nodeattrs.set_type_info(self.node, rtn_ti)
        
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
        call_node = self.node.get()
        assert isinstance(call_node, ast.Call)
        assert not hasattr(call_node, nodeattrs.ALT_NODE_ATTR)
        assert len(self.arg_nodes) > 0
        first_arg_node = self.arg_nodes[0]
        first_arg_node_type_info = self.ast_context.get_type_info_by_node(first_arg_node)
        assign_lhs_node = nodebuilder.identifier(first_arg_node.id)
        assign_node = ast.Assign()
        assign_node.targets = [assign_lhs_node]

        # we need to shallow copy the node so that when we set the ALT_NODE_ATTR
        # on self.node, it is really only set on self.node.
        # otherwise we get:
        # this node (n1) 's alt node -> assign_node (n2) -> assign_node.value (n1)
        # -> circular reference n1 -> n2 -> n1
        call_node = nodes.shallow_copy_node(call_node)
        assign_node.value = call_node
        # required so that this call node does not get rewritten again
        setattr(call_node, nodeattrs.REWRITTEN_NODE_ATTR, True)
        self._set_alt_node_attr(assign_node)

        self.ast_context.register_type_info_by_node(assign_lhs_node, first_arg_node_type_info)
        self.ast_context.register_type_info_by_node(call_node, first_arg_node_type_info)

        # the return type of the function is the same as the first argument
        # passed into the function, by definiton - we can link the types
        # together by using a late resolver
        rtn_type = context.TypeInfo.late_resolver(lambda ati: ati)
        # set the type info on the rhs node so we can find it again
        # this is required because the original node typically returns nothing
        # (l.append(... -> l = append(l, ...
        nodeattrs.set_type_info(call_node, rtn_type, allow_reset=True)

        # required because this node just created may be rewritten again
        # (elisp: assign -> setq call)
        self.ast_context.register_type_info_by_node(assign_node, rtn_type)


        assert not hasattr(assign_node, nodeattrs.ALT_NODE_ATTR)

        return self

    def rewrite_as_func_call(self, inst_1st=False, inst_node_attrs=[]):
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

        inst_node_attrs: node attributes that will be set on the <instance>
        node (which is rewritten as an argument node).
        """
        node = self.node.get()
        assert isinstance(node, ast.Call), "expected Call node but got %s" % node
        assert isinstance(node.func, ast.Attribute)
        inst_arg_node = node.func.value
        nodeattrs.set_node_attributes(inst_arg_node, inst_node_attrs)
        if inst_1st:
            self.prepend_arg(inst_arg_node)
        else:
            self.append_arg(inst_arg_node)
        # remove the attribute ref, keep the ref (function) name
        func_name = ast.Name()
        func_name.id = node.func.attr
        node.func = func_name

        # set the type info on the node, because we can't look up the
        # registered builtin function anymore
        type_info = self.ast_context.get_type_info_by_node(node)
        nodeattrs.set_type_info(node, type_info)
        
        return self

    def rewrite_as_attr_method_call(self):
        """
        Rewrites <method>(args + [<instance>]) as <instance>.<method>(args).

        This is the opposite of rewrite_as_func_call.
        """
        node = self.node.get()
        assert isinstance(node, ast.Call)
        assert isinstance(node.func, ast.Name), "got %s" % node.func
        assert len(self.arg_nodes) >= 0
        attr_node = ast.Attribute()
        # by default the first arg becomes the target instance
        # this is necessary for the case str1 == str2, which should be converted
        # to str1.equals(str2)
        attr_node.value = node.args[0]
        attr_node.attr = node.func.id
        del node.args[0]
        node.func = attr_node

        rtn_ti = self.ast_context.get_type_info_by_node(node)
        nodeattrs.set_type_info(node, rtn_ti)

        return self

    def call_on_target(self, method_name, keep_args=True):
        """
        Calls the specific method on the contextual target (instance).

        This replaces the existing node.

        For example:
            d[1] = 2 # d is the target
          ->
            d.put(1, 2);
        """
        assert self._target_node is not None
        args = self.arg_nodes if keep_args else []
        attr_call_node = nodebuilder.attr_call(self._target_node, method_name, args)
        self._set_alt_node_attr(attr_call_node)
        rtn_type_info = self.ast_context.get_type_info_by_node(self.node)
        self.ast_context.register_type_info_by_node(attr_call_node, rtn_type_info)
        nodeattrs.set_type_info(attr_call_node, rtn_type_info)

        # move into _set_alt_node
        if hasattr(self.node, nodeattrs.CONTAINER_MD_ATTR):
            v = nodeattrs.get_attr(self.node, nodeattrs.CONTAINER_MD_ATTR)
            nodeattrs.set_attr(attr_call_node, nodeattrs.CONTAINER_MD_ATTR, v)

        return self

    def call_with_target_as_arg(self, func_name, target_as_first_arg=True):
        """
        Calls the specific function and and makes the contextual target
        (instance) either the first or the last argument.

        This replaces the existing node.

        For example:
            d[1] = 2 # d is the target, '=' is self.node
          ->
            (puthash "test" 2 d)
        """
        assert self._target_node is not None
        arg_nodes = [self._target_node] + self.arg_nodes if target_as_first_arg else self.arg_nodes + [self._target_node]
        call_node = nodebuilder.call(func_name, arg_nodes)

        # propagate the existing rtn type to the new node
        rtn_type_info = self.ast_context.get_type_info_by_node(self.node)
        nodeattrs.set_type_info(call_node, rtn_type_info)
        self.ast_context.register_type_info_by_node(call_node, rtn_type_info)
        self._set_alt_node_attr(call_node)

        # move into _set_alt_node
        if hasattr(self.node, nodeattrs.CONTAINER_MD_ATTR):
            cmd = nodeattrs.get_attr(self.node, nodeattrs.CONTAINER_MD_ATTR)
            if not target_as_first_arg:
                # we want: (target, key, value)
                cmd.update_transfomer(lambda key, value, target: (target, key, value))
            nodeattrs.set_attr(call_node, nodeattrs.CONTAINER_MD_ATTR, cmd)
        
        return self

    def chain_method_call(self, method_name):
        """
        Chains the specified name, as a method call, to the current node.

        For example:

          ident -> ident.foo()

          Initial state:
              Name(id='ident', ctx=Load())
          Rewritten state:
              Call(func=
                Attribute(value=
                  Name(id='ident', ctx=Load()),
                attr='foo', ctx=Load()),
              args=[], keywords=[])


          f1() -> f1().m1()

          Initial state:
              Call(func=Name(id='f', ctx=Load()), args=[], keywords=[])
          Rewritten state:
              Call(func=
                Attribute(value=
                  Call(func=
                    Name(id='f', ctx=Load()),
                  args=[], keywords=[]),
                attr='m1', ctx=Load()),
              args=[], keywords=[])
        """
        assert isinstance(self.node, (ast.Call, ast.Name)), "a method can only be chained to another function/method call or to an identifier"
        # this is the node we want to make the call "on", ie f() -> f().m1()
        target_node = self.node.get()
        target_node_type_info = self.ast_context.get_type_info_by_node(target_node)

        # we cannot use the original target node instance because it remains
        # at its current position in the AST and we set ALT_NODE on it
        cloned_target_node = nodes.shallow_copy_node(target_node)
        self.ast_context.register_type_info_by_node(cloned_target_node,
                                                    target_node_type_info)


        # by default, the new chained call evaluates to the same type as the
        # original node
        chained_method_rtn_type = target_node_type_info
        if isinstance(method_name, context.Function):
            f = method_name
            method_name = f.name
            chained_method_rtn_type = f.get_rtn_type_info()

        chained_method_call = nodebuilder.attr_call(cloned_target_node,
            method_name, node_attrs=[nodeattrs.REWRITTEN_NODE_ATTR])

        self.ast_context.register_type_info_by_node(chained_method_call,
                                                    chained_method_rtn_type)
        # required so that the next time typevisitor runs, it finds the return
        # type for this materialized call
        nodeattrs.set_type_info(chained_method_call, chained_method_rtn_type)

        self._set_alt_node_attr(chained_method_call)
        
        return self

    def replace_node_with(self, rewriter_or_node, keep_args=True,
                          current_node_becomes_singleton_arg=False):
        assert isinstance(rewriter_or_node, (ASTRewriter, ast.AST)),\
            "expected an an ASTRewriter or node instance"
        current_node = self.node.get()
        if isinstance(rewriter_or_node, ASTRewriter):
            rewriter = rewriter_or_node
        else:
            rewriter = self.wrap(rewriter_or_node)
        target_node = rewriter.node
        type_info = self.ast_context.get_type_info_by_node(current_node)
        self.ast_context.register_type_info_by_node(target_node, type_info)
        if current_node_becomes_singleton_arg:
            keep_args = False
            target_node.args = []
            arg_node = nodes.shallow_copy_node(current_node)
            setattr(arg_node, nodeattrs.REWRITTEN_NODE_ATTR, True)
            target_node.args.append(arg_node)
        if keep_args:
            target_node.args = []
            target_node.args += rewriter._prepended_args
            target_node.args += self._arg_nodes
            target_node.args += rewriter._appended_args
        self._set_alt_node_attr(target_node)

        nodeattrs.set_type_info(target_node, type_info)

        if isinstance(current_node, ast.Assign):
            lhs_node = current_node.targets[0].get()
            setattr(target_node, nodeattrs.ASSIGN_LHS_NODE_ATTR, lhs_node)
            lhs_type_info = self.ast_context.get_type_info_by_node(lhs_node)
            nodeattrs.set_type_info(lhs_node, lhs_type_info)

        return self

    def _get_for_loop_range_nodes(self):
        assert isinstance(self.node, ast.For)
        rhs = self.node.iter
        if self._is_calling(rhs, "range"):
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
        rhs = self.node.iter
        if self._is_calling(rhs, "enumerate"):
            return rhs.args[0].get()
        return None

    def _is_calling(self, node, func_name):
        return (isinstance(node, ast.Call) and
                isinstance(node.func, ast.Name) and
                node.func.id == func_name)


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
                counter_var_name = self.ast_context.get_unique_identifier_name("i")
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

        # iter_node is what is being iterated over - we need to have len()
        # of that in the for loop conditional and then also get an element
        # out of it using the counter_var - if iter_node is not a ast.Name,
        # we will add an assignment to a temp var above the for loop:
        #
        # for i = 0; i < len(foo()); i++) { # foo() is the iter_node
        #     t = foo()[i]
        # }
        # ->
        # x = foo()
        # for i = 0; i < len(x); i++) {
        #     t = x[i]
        # }
        #
        # if iter_node is an ast.Name, we can just use it - to keep the code
        # as simple as possible, we will use the variable called
        # iter_var_name to cover both cases
        iter_ti = self.ast_context.get_type_info_by_node(iter_node)
        if isinstance(iter_node, ast.Name):
            iter_var_name = iter_node.id
        else:
            iter_var_name = self.ast_context.get_unique_identifier_name()
            iter_node_copy = nodes.deep_copy_node(iter_node, self.ast_context)
            iter_var_assign = nodebuilder.assignment(iter_var_name, iter_node_copy)
            nodebuilder.insert_node_above(iter_var_assign, self.parent_body, self.node)
            self.ast_context.register_type_info_by_node(iter_var_assign.targets[0], iter_ti)

        end_node = self.call(context.LEN_BUILTIN).append_arg(self.ident(iter_var_name, iter_ti)).node
        self.ast_context.register_type_info_by_node(end_node, int_ti)

        setattr(self.node, nodeattrs.FOR_LOOP_C_STYLE_INIT_NODE, init_node)
        setattr(self.node, nodeattrs.FOR_LOOP_C_STYLE_COND_NODE,
                nodebuilder.compare(counter_var_name, "<", end_node))
        setattr(self.node, nodeattrs.FOR_LOOP_C_STYLE_EXPR_NODE,
                nodebuilder.reassignment(counter_var_name, 1, "+"))

        iter_var_ref = nodebuilder.identifier(iter_var_name)
        self.ast_context.register_type_info_by_node(iter_var_ref, iter_ti)
        sub_node = nodebuilder.subscript_list(iter_var_ref, counter_var_name)
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
                nodebuilder.compare(target_node_name, op, end_node))
        setattr(self.node, nodeattrs.FOR_LOOP_C_STYLE_EXPR_NODE,
                nodebuilder.reassignment(target_node_name, step_node, "+"))

    def rewrite_as_if_stmt(self):
        """
        This method rewrites if-expression usage in specific contexts as
        regular if-statements.

        Examples:

        assignment:
            a = 3 if 0 == 0 else 2
            ->
            if 0 == 0:
                a = 3
            else:
                a = 2

        return:
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
        if_ti = None

        if isinstance(if_expr_parent_node, ast.Lambda):
            # f = lambda: 1 if 0 == 0 else 2
            # some special casing for this:
            # - add explicit return stmts (should another visitor do this?)
            # - set the rtn type on the if stmt so we can find it later
            body_node = nodebuilder.rtn(org_body)
            orelse_node = nodebuilder.rtn(org_orelse)
            if_ti = self.ast_context.get_type_info_by_node(org_body)
            # we need to set the alternative node on the original IfExpr node
            if_expr_parent_node = self.node
        else:
            ctx = self.ast_context
            body_node = nodes.shallow_copy_node(if_expr_parent_node, ctx)
            body_node.value = org_body
            orelse_node = nodes.shallow_copy_node(if_expr_parent_node, ctx)
            orelse_node.value = org_orelse

        if_node = nodebuilder.if_stmt(
            body=body_node, test=org_test, orelse=orelse_node)
        if if_ti is not None:
            # special case for re-written lambda expressions
            nodeattrs.set_type_info(if_node, if_ti)
        setattr(if_expr_parent_node, nodeattrs.ALT_NODE_ATTR, if_node)

    def insert_above(self, rewriter):
        assert isinstance(rewriter, ASTRewriter)
        nodebuilder.insert_node_above(rewriter.node, self.parent_body, self.node)
        return self

    def add_type_info(self, *additional_type_info):
        """
        Associates an additional type_info with the current node.
        """
        updated_tis = []
        for ti in additional_type_info:
            if not isinstance(ti, context.TypeInfo):
                ti = context.TypeInfo(ti)
            updated_tis.append(ti)
        additional_type_info = updated_tis

        associate_with_node = False
        type_info = self.ast_context.lookup_type_info_by_node(self.node)
        if type_info is None:
            associate_with_node = True
            if len(additional_type_info) == 1:
                type_info = additional_type_info[0]
            else:
                type_info = context.TypeInfo.tuple()
        else:
            if not type_info.is_container:
                associate_with_node = True
                type_info = context.TypeInfo.tuple().of(type_info)

        for ti in additional_type_info:
            type_info.register_contained_type(type_info.num_contained_type_infos, ti)

        if associate_with_node:
            self.ast_context.register_type_info_by_node(self.node, type_info)
            nodeattrs.set_type_info(self.node, type_info, allow_reset=True)
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

    def _set_alt_node_attr(self, alt_node):
        nodeattrs.set_attr(self.node, nodeattrs.ALT_NODE_ATTR, alt_node, overwrite=True)

    def _add_arg(self, append, args):
        for arg in args:
            arg_node = self._get_node(arg)
            if hasattr(arg_node, nodeattrs.ALT_NODE_ATTR):
                alt_node = getattr(arg_node, nodeattrs.ALT_NODE_ATTR)
            node = self.node.get()
            arg_node = arg_node.get()
            if append:
                node.args.append(arg_node)
                self._appended_args.append(arg_node)
            else:
                node.args.insert(0, arg_node)
                self._prepended_args.append(arg_node)
        return self
