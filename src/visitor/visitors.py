from lang.target import rewrite
from lang.target import targetlanguage
from lang.target import targets
from visitor import visitor
import ast
import copy
import lang.astrewriter as astrewriter
import lang.internal.typeinfo as tim
import lang.nodebuilder as nodebuilder
import lang.nodes as nodes
import types
import visitor.context as context
import visitor.nodeattrs as nodeattrs


class _CommonStateVisitor(visitor.NoopNodeVisitor):
    """
    Boolean state based on nodes being visited.
    """

    def __init__(self, ast_context, target):
        super().__init__()
        self.ast_context = ast_context
        self.target = target
        self.assign_visiting_lhs = False
        self.assign_visiting_rhs = False
        self.loop_visiting_lhs = False
        self.loop_visiting_rhs = False
        self.visiting_rtn = False

        # needs to be a stack for nested func names, for example:
        # print("foo".startswith("f"))
        self.func_name_stack = []
        self.parent_node_stack = [] # call/attr

    @property
    def visiting_func(self):
        return False if len(self.parent_node_stack) == 0 else isinstance(self.parent_node_stack[-1], ast.Call)

    @property
    def visiting_attr(self):
        return False if len(self.parent_node_stack) == 0 else isinstance(self.parent_node_stack[-1], ast.Attribute)

    # returns the current func name
    def call(self, node, num_children_visited):
        super().call(node, num_children_visited)
        if num_children_visited == 0:
            self.parent_node_stack.append(node)
        elif num_children_visited == 1:
            self.parent_node_stack.pop()
        elif num_children_visited == -1:
            name =  self.func_name_stack.pop()
            return name
        return None

    def assign(self, node, num_children_visited):
        super().assign(node, num_children_visited)
        assert len(node.targets) == 1
        lhs = node.targets[0]
        if isinstance(lhs, ast.Subscript):
            # d["foo"] = blah - special syntax - skip
            # (this same check exists in most visitors)
            pass
        else:
            if num_children_visited == 0:
                assert not self.assign_visiting_lhs
                # the assert below is too simplistic, there can be nested
                # rhs assignments:
                # f = lambda: a = 3
                # kinda weird in python, but will happen when lamda expressions
                # in combination with if-expr are rewritten:
                # f = lambda: 2 if a == 2 else 1
                # -> in Golang, this is an anonymous function with a "regular"
                # if statement that has assignments to a temp var in each branch
                #assert not self.assign_visiting_rhs
                self.assign_visiting_lhs = True
                self.assign_visiting_rhs = False
            elif num_children_visited != -1:
                self.assign_visiting_lhs = False
                self.assign_visiting_rhs = True
            else: # num_children_visited == -1
                self.assign_visiting_rhs = False

    def attr(self, node, num_children_visited):
        if num_children_visited == 0:
            self.parent_node_stack.append(node)
        if num_children_visited == -1:
            self.parent_node_stack.pop()
            if self.visiting_func:
                self.func_name_stack.append(node.attr)

    def loop_for(self, node, num_children_visited, is_foreach):
        super().loop_for(node, num_children_visited, is_foreach)
        if num_children_visited == 0:
            assert not self.loop_visiting_lhs
            assert not self.loop_visiting_rhs
            self.loop_visiting_lhs = True
        elif num_children_visited == 1:
            assert self.loop_visiting_lhs
            assert not self.loop_visiting_rhs
            self.loop_visiting_lhs = False
            self.loop_visiting_rhs = True
        elif num_children_visited == 2:
            assert not self.loop_visiting_lhs
            assert self.loop_visiting_rhs
            self.loop_visiting_rhs = False

    def name(self, node, num_children_visited):
        super().name(node, num_children_visited)
        if self.visiting_func:
            self.func_name_stack.append(node.id)

    def rtn(self, node, num_children_visited):
        super().rtn(node, num_children_visited)
        if num_children_visited == 0:
            assert not self.visiting_rtn
            self.visiting_rtn = True
        elif num_children_visited == -1:
            assert self.visiting_rtn
            self.visiting_rtn = False

    def _reset(self):
        self.assign_visiting_lhs = False
        self.assign_visiting_rhs = False
        self.loop_visiting_lhs = False
        self.loop_visiting_rhs = False
        self.func_name_stack = []
        self.parent_node_stack = []


class BodyParentNodeVisitor(visitor.NoopNodeVisitor):
    """
    Keeps track of parent blocks/bodies (in the sense of an if-stmt body for ex)
    and gives access to them. A parent "body" is just a list of ast nodes that
    can be modified (added to, typically).

    For Don: this class is meant to be inherited from. This is not
    implementation inheritance because this class implements visitor methods.
    """

    def __init__(self):
        super().__init__()
        self.parent_body_stack = []

    @property
    def parent_body(self):
        return self.parent_body_stack[-1]

    @property
    def grandparent_body(self):
        return self.parent_body_stack[-2]

    def block(self, node, num_children_visited, is_root_block, body):
        super().block(node, num_children_visited, is_root_block, body)
        assert isinstance(body, (list, tuple))
        if num_children_visited == 0:
            self.parent_body_stack.append(body)
        elif num_children_visited == -1:
            self.parent_body_stack.pop()


class ContainerTypeVisitor(visitor.NoopNodeVisitor):

    def assign(self, node, num_children_visited):
        super().assign(node, num_children_visited)
        if num_children_visited == -1:
            assert len(node.targets) == 1
            lhs = node.targets[0].get() # get() because array access
            if isinstance(lhs, ast.Subscript):
                # dict assignment
                self._set_container_md(node, context.DictContainerMetadata())

    def call(self, node, num_children_visited):
        super().call(node, num_children_visited)
        if num_children_visited == -1:
            if isinstance(node.func, ast.Attribute) and node.func.attr == "append":
                # we don't have types, in the end maybe this needs
                # to move into type visitor? do we get anything from having
                # this code here?

                # append to list (?)
                self._set_container_md(node, context.ListContainerMetadata())

    def _set_container_md(self, node, md):
        if not nodeattrs.has_container_md(node):
            nodeattrs.set_container_md(node, md)


class FuncCallVisitor(_CommonStateVisitor, BodyParentNodeVisitor):
    """
    Executes rewrite rules on the AST.

    TODO rename this class.
    """
    def __init__(self, ast_context, target):
        super().__init__(ast_context, target)
        self._keep_revisiting = False

    @property
    def leave_early(self):
        return self._keep_revisiting

    @property
    def should_revisit(self):
        if self._keep_revisiting:
            self._keep_revisiting = False
            super()._reset()
            return True
        return False

    def assign(self, node, num_children_visited):
        if not hasattr(node, nodeattrs.REWRITTEN_NODE_ATTR):
            super().assign(node, num_children_visited)
            if num_children_visited == -1:
                assert len(node.targets) == 1
                lhs = node.targets[0]
                if isinstance(lhs, ast.Subscript):
                    # Python "add to dict" syntax: d[key] = value - provide a
                    # rewrite hook at the assigment node level
                    # (similar checks exist in other visitors)
                    # lhs.value: dict instance
                    # lhs.slice: key
                    # node.value: value
                    self._handle_rewrite(rewrite.Operator.DICT_ASSIGNMENT,
                                         lhs.value, node,
                                         arg_nodes=[lhs.slice, node.value])
                else:
                    self._handle_rewrite(rewrite.Operator.ASSIGNMENT,
                                         None, node,
                                         arg_nodes=[lhs.get(), node.value])

    def assign_aug(self, node, num_children_visited):
        super().assign_aug(node, num_children_visited)
        if num_children_visited == -1:
            target = rewrite.Operator.forNode(node.op)
            target = target.assign_to_self
            self._handle_rewrite(target, None, node, arg_nodes=[node.target, node.value])

    def unaryop(self, node, num_children_visited):
        super().unaryop(node, num_children_visited)
        if num_children_visited == -1:
            target = rewrite.Operator.forNode(node.op)
            self._handle_rewrite(target, None, node, [node.operand])

    def binop(self, node, num_children_visited):
        super().binop(node, num_children_visited)
        if num_children_visited == -1:
            target = rewrite.Operator.forNode(node.op)
            self._handle_rewrite(target, None, node, [node.left, node.right])

    def boolop(self, node, num_children_visited):
        super().boolop(node, num_children_visited)
        if num_children_visited == -1:
            target = rewrite.Operator.forNode(node.op)
            self._handle_rewrite(target, None, node, node.values)

    def compare(self, node, num_children_visited):
        super().compare(node, num_children_visited)
        if num_children_visited == -1:
            assert len(node.ops) == 1
            assert len(node.comparators) == 1
            target = rewrite.Operator.forNode(node.ops[0])
            self._handle_rewrite(target, None, node, [node.left, node.comparators[0]])

    def attr(self, node, num_children_visited):
        super().attr(node, num_children_visited)
        if num_children_visited == -1:
            attr_name = node.attr
            target_node = node.value
            args = []
            self._handle_rewrite(attr_name, target_node, node, args)

    def call(self, node, num_children_visited):
        func_name = super().call(node, num_children_visited)
        if num_children_visited == -1:
            assert func_name is not None
            target_node = None
            if isinstance(node.func, ast.Attribute):
                target_node = node.func.value
            self._handle_rewrite(func_name, target_node, node, node.args)

    def cond_if(self, node, num_children_visited):
        super().cond_if(node, num_children_visited)
        if num_children_visited == -1:
            self._handle_rewrite(rewrite.Keyword.IF,
                                 None, node, arg_nodes=[node.test])

    def cond_if_expr(self, node, num_children_visited):
        super().cond_if_expr(node, num_children_visited)
        if num_children_visited == -1:
            self._handle_rewrite(rewrite.Keyword.IF_EXPR,
                                 None, node, arg_nodes=[node.test])

    def list_comp(self, node, num_children_visited):
        super().list_comp(node, num_children_visited)
        if num_children_visited == -1:
            self._handle_rewrite(rewrite.Keyword.LIST_COMP,
                                 None, node, arg_nodes=[])

    def loop_for(self, node, num_children_visited, is_foreach):
        super().loop_for(node, num_children_visited, is_foreach)
        if num_children_visited == -1:
            self._handle_rewrite(rewrite.Keyword.FOR,
                                 None, node, arg_nodes=[node.target, node.iter])

    def subscript(self, node, num_children_visited):
        super().subscript(node, num_children_visited)
        if num_children_visited == -1:
            target_node = node.value
            target_type = self.ast_context.get_type_info_by_node(target_node).value_type
            if target_type is str:
                assert node.slice.lower is not None, "implement me!"
                arg_nodes = [node.slice.lower]
                if node.slice.upper is not None:
                    arg_nodes.append(node.slice.upper)
            else:
                arg_nodes = [node.slice]
            self._handle_rewrite(rewrite.Operator.SUBSCRIPT,
                                 target_node, node, arg_nodes)

    def _handle_rewrite(self, func_name, target_node, node, arg_nodes):
        # TODO rename func_name to ...
        if hasattr(node, nodeattrs.REWRITTEN_NODE_ATTR):
            return
        if isinstance(func_name, rewrite.RewriteTarget):
            func_name = func_name.name
        arg_nodes = [a.get() for a in arg_nodes]
        target_type = None
        if target_node is None:
            if len(arg_nodes) > 0:
                target_type_info = self.ast_context.lookup_type_info_by_node(arg_nodes[0])
                if target_type_info is None:
                    # some nodes do not have a type, for example function
                    # definitions - their type should be "function" by we don't
                    # support those yet
                    pass
                else:
                    target_type = target_type_info.value_type
        else:
            target_type_info = self.ast_context.get_type_info_by_node(target_node)
            target_type = target_type_info.value_type
        rewrite_target = self._lookup_rewrite_target(func_name, target_type, node)
        if rewrite_target is not None:
            self.ast_context.register_imports(rewrite_target.imports)
            args = []
            for arg_node in arg_nodes:
                type_info = self.ast_context.lookup_type_info_by_node(arg_node)
                assert type_info is not None, "unable to lookup type info for function %s: arg %s" % (func_name, arg_node)
                args.append(targetlanguage.Argument(arg_node, type_info.value_type))
            rw = astrewriter.ASTRewriter(node,
                                         arg_nodes,
                                         self.ast_context,
                                         self.parent_body,
                                         target_node)

            # the actual AST rewriting happens here:
            if rewrite_target.target_name is not None:
                rw.rename(rewrite_target.target_name)
            if rewrite_target.function_rewrite is not None:
                rewrite_target.function_rewrite(args, rw)
            self._keep_revisiting = True
            setattr(node, nodeattrs.REWRITTEN_NODE_ATTR, True)

    def _lookup_rewrite_target(self, func_name, target_type, node):
        # currently we distinguish between function/method == "Call"
        # rewrites and attribute rewrites, for example:
        # os.path.join() <- call
        # os.path.sep <- attr
        attr_path = None
        if target_type is types.ModuleType:
            attr_path = nodes.get_attr_path(node)
        key = self.target.get_function_lookup_key(func_name, target_type, attr_path, type(node))
        if key not in self.target.rewrite_rules:
            key = self.target.get_function_lookup_key(func_name, target_type=None, ast_path=attr_path, target_node_type=type(node))
        if key in self.target.rewrite_rules:
            return self.target.rewrite_rules[key]
        if rewrite.ALL in self.target.rewrite_rules:
            # this is the special wildcard target
            return self.target.rewrite_rules[rewrite.ALL]
        return None


class IfExprRewriter(visitor.NoopNodeVisitor):
    """
    This visitor translates Python if-expressions, such as "1 if p == 1 else 2"
    to regular, boring if statements. The complication here is the change from
    "expression" to "statement"; since if statements do not evaluate to anything
    we follow this strategy:
    -  Detect the root if-expr nodes: if expression can be nested, for example:
       "1 if p == 1 else 2 if p == 3 else ...". We only care about the root
       nodes, so that's "1 if p == 1 else ..." in this example
    - For each root expression, pull it up into the parent block and make it a
      top-level if-stmt, assigning the result of the branches to a temp variable
    - Use the temp variable where the if-expr was used before it was pulled up

    Examples:

      p = 0
      my_function(1 if p == 0 else 2 if p == 1 else 3)
    ->
      p = 0
      if p == 0:
          v = 1
      else:
          if p == 1:
              v = 2
          else:
              v = 3
      my_function(v)


      def my_function(i):
          return 1 if i == 0 else 2
    ->
      def my_function(i):
          if i == 0:
              v = 1
          else:
              v = 2
          return v


    If the if-expr is used as "assignment-only", we do not create a temp var
    and "just translate":

      p = 0
      a = 1 if p == 0 else 2
    ->
      p = 0
      if p == 0:
          a = 1
      else:
          a = 2
    """

    IF_EXPR_MARKER = "if-expr"
    NESTED_IF_EXPR_MARKER = "nested_if-expr"
    TMP_ASSIGN_NODE_MARKER = "tmp-assign"

    def __init__(self, ast_context):
        super().__init__()
        self.ast_context = ast_context
        self.root_if_expr_nodes = []
        self.tmp_var_name = None
        # multiple passes (visits) to clarify:
        # 0: find top level if-expr nodes and mark them
        # 1: pull those marked if expressions into the body
        # 2: rewrite all if-expr nodes as boring if statements
        self.visit_iteration = 0

    def cond_if_expr(self, node, num_children_visited):
        super().cond_if_expr(node, num_children_visited)
        if self.visit_iteration == 0:
            if num_children_visited == 0:
                if nodeattrs.has_attr(node, IfExprRewriter.NESTED_IF_EXPR_MARKER):
                    pass
                else:
                    # mark top level if expr nodes
                    nodeattrs.set_attr(node, IfExprRewriter.IF_EXPR_MARKER)
                    self.root_if_expr_nodes.append(node)
                if isinstance(node.orelse, ast.IfExp):
                    nodeattrs.set_attr(node.orelse, IfExprRewriter.NESTED_IF_EXPR_MARKER)
        elif self.visit_iteration == 2:
            if num_children_visited == -1:
                if nodeattrs.has_attr(node, IfExprRewriter.TMP_ASSIGN_NODE_MARKER):
                    # dealt with in assign visitor method below
                    pass
                else:
                    # nested if expr
                    if_stmt_node = self._rewrite_as_if_stmt(node)
                    nodeattrs.set_rewritten_node(node, if_stmt_node)

    def assign(self, node, num_children_visited):
        super().assign(node, num_children_visited)
        if self.visit_iteration == 2:
            if nodeattrs.has_attr(node, IfExprRewriter.TMP_ASSIGN_NODE_MARKER):
                # this assignment node was created by
                # nodes.extract_expressions_with_attr (see def block below)
                if num_children_visited == 0:
                    assert self.tmp_var_name is None
                    self.tmp_var_name = node.targets[0].id
                    nodeattrs.set_attr(node.value, IfExprRewriter.TMP_ASSIGN_NODE_MARKER)
                elif num_children_visited == -1:
                    if_stmt_node = self._rewrite_as_if_stmt(node.value)
                    nodeattrs.set_rewritten_node(node, if_stmt_node)
                    nodeattrs.rm_attr(node, IfExprRewriter.TMP_ASSIGN_NODE_MARKER)
                    nodeattrs.rm_attr(node.value, IfExprRewriter.TMP_ASSIGN_NODE_MARKER)
                    self.tmp_var_name = None

    def block(self, node, num_children_visited, is_root_block, body):
        super().block(node, num_children_visited, is_root_block, body)
        if self.visit_iteration == 1:
            if num_children_visited == -1:
                for n in body:
                    assign_nodes = nodes.extract_expressions_with_attr(
                        n.get(), body, IfExprRewriter.IF_EXPR_MARKER,
                        self.ast_context, remove_attr=True)
                    for assign_node in assign_nodes:
                        nodeattrs.set_attr(
                            assign_node, IfExprRewriter.TMP_ASSIGN_NODE_MARKER)

    @property
    def should_revisit(self):
        if self.visit_iteration < 2:
            self.visit_iteration += 1
            return True

    def _rewrite_as_if_stmt(self, node):
        assert isinstance(node, ast.IfExp), type(node)
        assert self.tmp_var_name is not None
        if_body = nodebuilder.assignment(self.tmp_var_name, nodes.shallow_copy_node(node.body))
        if_orelse = nodes.shallow_copy_node(node.orelse)
        if not isinstance(if_orelse, ast.If):
            # not a nested if exp (assumes it has been rewritten!)
            # ie it is no longer an ast.IfExp
            if_orelse = nodebuilder.assignment(self.tmp_var_name, if_orelse)
        return nodebuilder.if_stmt(
            nodes.shallow_copy_node(node.test), if_body, [if_orelse])


class ListCompRewriter(BodyParentNodeVisitor):
    """
    This is similar to the IfExprRewriter.

    TODO - this doesn't handle nested list-comp expressions yet.
    This has to be done similarly, again, to IfExprRewriter above.

    If there are more things like this, can we generalize to
    ExpressionRewriter?
    """

    
    LIST_COMP_EXPR_MARKER = "if-expr"
    TMP_ASSIGN_NODE_MARKER = "tmp-assign"

    def __init__(self, ast_context):
        super().__init__()
        self.ast_context = ast_context
        self.root_if_expr_nodes = []
        self.tmp_var_name = None
        # multiple passes (visits) to clarify:
        # 0: find list comp nodes and mark them
        # 1: pull those marked list comps into the body
        # 2: rewrite those marked list comp nodes as for loops
        self.visit_iteration = 0

    def list_comp(self, node, num_children_visited):
        super().list_comp(node, num_children_visited)
        if self.visit_iteration == 0:
            if num_children_visited == 0:
                nodeattrs.set_attr(node, ListCompRewriter.LIST_COMP_EXPR_MARKER)

    def assign(self, node, num_children_visited):
        super().assign(node, num_children_visited)
        if self.visit_iteration == 2:
            if nodeattrs.has_attr(node, ListCompRewriter.TMP_ASSIGN_NODE_MARKER):
                # this assignment node was created by
                # nodes.extract_expressions_with_attr (see def block below)
                if num_children_visited == 0:
                    assert self.tmp_var_name is None
                    self.tmp_var_name = node.targets[0].id
                elif num_children_visited == -1:
                    rhs, for_loop = self._rewrite_as_for_loop(node.value)
                    nodeattrs.set_rewritten_node(node.value, rhs)
                    nodes.insert_node_below(for_loop, self.parent_body, node)
                    nodeattrs.rm_attr(node, ListCompRewriter.TMP_ASSIGN_NODE_MARKER)
                    self.tmp_var_name = None

    def block(self, node, num_children_visited, is_root_block, body):
        super().block(node, num_children_visited, is_root_block, body)
        if self.visit_iteration == 1:
            if num_children_visited == -1:
                for n in body:
                    assign_nodes = nodes.extract_expressions_with_attr(
                        n.get(), body, ListCompRewriter.LIST_COMP_EXPR_MARKER,
                        self.ast_context, remove_attr=True)
                    for assign_node in assign_nodes:
                        nodeattrs.set_attr(
                            assign_node, ListCompRewriter.TMP_ASSIGN_NODE_MARKER)

    @property
    def should_revisit(self):
        if self.visit_iteration < 2:
            self.visit_iteration += 1
            return True

    def _rewrite_as_for_loop(self, node):
        """
          t = [i for i in l] # self.tmp_var_name is t
        ->
          t = []
          for i in l:
              t.append(i)

        This re-rewrite requires initializing the new list (the lhs of the
        assignment) first, so we cannot return a single node here that can
        just replace the original assignment node.
        Note that this is slightly more complicated to how the if expression
        is rewritten.

        Returns 2 nodes:
          - the updated rhs, which is just initializing t to an empty list
          - the for loop node
        """
        assert isinstance(node, ast.ListComp), type(node)
        assert self.tmp_var_name is not None
        assert len(node.generators) == 1
        target_node = node.generators[0].target # the iterating var
        iter_node = node.generators[0].iter # what we're iterating over
        append_call_node = nodebuilder.attr_call(
            nodebuilder.identifier(self.tmp_var_name),
            "append",
            [node.elt])
        body_node = append_call_node
        if len(node.generators[0].ifs) > 0:
            body_node = nodebuilder.if_stmt(node.generators[0].ifs[0], body_node)
        for_node = nodebuilder.for_loop(target_node, iter_node, body=body_node)
        return nodebuilder.list(), for_node


class IfExprToTernaryRewriter(visitor.NoopNodeVisitor):
    """
    Rewrites a Python style if-expression as  a c-style ternary if-expression.

    This rewriter needs to run last, towards the end, as it breaks the
    TypeVisitor. It is really more of a syntax manipulation than an ast rewrite,
    but implementing it as an ast rewrite makes this logic simple.
    """
    def cond_if_expr(self, node, num_children_visited):
        """
        a = 3 if 0 == 0 else 2
        3: node.body
        0 == 0: node.test
        2: node.orelse
        """
        super().cond_if_expr(node, num_children_visited)
        if num_children_visited == -1:
            body = node.body
            test = node.test
            node.body = test
            node.test = body


class BlockScopePuller(_CommonStateVisitor):
    """
    Pulls declarations made in a block and referenced outside out of the block.

    if 1 == 1:
        name = "water"
    print(name)

    -> 

    name = None
    if 1 == 1:
        name = "water"
    print(name)
    """
    def __init__(self, ast_context, target):
        super().__init__(ast_context, target)
        # tracks identifiers that need to be declared in a parent scope
        self.ident_names = set()

    def name(self, node, num_children_visited):
        super().name(node, num_children_visited)
        if num_children_visited == 0:
            if not self.visiting_func:
                scope = self.ast_context.current_scope.get()
                if not scope.is_declaration_node(node):
                    if not scope.has_been_declared(node.id):
                        self.ident_names.add(node.id)

    def on_scope_released(self, scope):
        if scope.has_namespace or not scope.has_parent:
            for ident_name in self.ident_names:
                decl_node = nodebuilder.assignment(ident_name, None)
                declaring_scope = scope.get_declaring_child_scopes(ident_name)[0]
                nodes.insert_node_above(decl_node, scope.ast_node.body,
                                        declaring_scope.ast_node)
            self.ident_names = set()


class PointerVisitor(visitor.NoopNodeVisitor):
    """
    Marks function boundaries, argument and rtn ast nodes, as being pointers.
    This is picked up by visitor.typevisitor, when typevisitor runs after this
    visitor.

    This assumes that all types start out as value types when created, except
    types of type type (classes). This is to hack around how we deal with
    instantiation in Golang.
    """

    def __init__(self, ast_context, pointer_types):
        super().__init__()
        self.ast_context = ast_context
        self.pointer_types = pointer_types

    def funcdef(self, node, num_children_visited):
        """
        Iterates over user defined functions and changes reference types to
        pointers.
        """
        super().funcdef(node, num_children_visited)
        if num_children_visited == -1:
            func = nodeattrs.get_function(node)
            arg_nodes_start_index = nodes.get_argument_signature_start_index(func.is_method)
            for i, arg_ti in enumerate(func.arg_type_infos):
                if arg_ti.value_type in self.pointer_types:
                    arg_node_index = i + arg_nodes_start_index
                    arg_node = node.args.args[arg_node_index].get()
                    # mark arg node as being a pointer, this is used in type
                    # visitor to update the associated type info
                    nodeattrs.set_attr(arg_node, nodeattrs.IS_POINTER_NODE_ATTR)

    def rtn(self, node, num_children_visited):
        super().rtn(node, num_children_visited)
        if num_children_visited == -1:
            ti = self.ast_context.get_type_info_by_node(node)
            if ti.value_type in self.pointer_types:
                # mark rtn node as being a pointer, this is used in type
                # visitor to update the associated type info
                nodeattrs.set_attr(node, nodeattrs.IS_POINTER_NODE_ATTR)

    def classdef(self, node, num_children_visited):
        super().classdef(node, num_children_visited)
        if num_children_visited == -1:
            if type in self.pointer_types:
                nodeattrs.set_attr(node, nodeattrs.IS_POINTER_NODE_ATTR)


class PointerHandlerVisitor(BodyParentNodeVisitor):
    """
    Adds pointer dereference (*) and address of (&) operators.
    """

    def __init__(self, ast_context):
        super().__init__()
        self.ast_context = ast_context

    def call(self, node, num_children_visited):
        """
        Processes callsites of user defined functions and makes sure that
        pointers are passed when required.
        """
        super().call(node, num_children_visited)
        if num_children_visited == -1:
            func = nodeattrs.get_function(node, must_exist=True)
            if func.is_builtin:
                # this is obviously wrong, but for now we assume that builtins
                # don't want pointers
                for n in node.args:
                    n = n.get() # iteration
                    ti = self.ast_context.get_type_info_by_node(n)
                    if ti.is_pointer:
                        nodeattrs.set_attr(n, nodeattrs.DEREF_NODE_MD)
            else:
                num_caller_args = len(node.args)
                assert len(func.arg_type_infos) == num_caller_args, "the function %s has %s argument types but the caller has %s arguments" % (func, len(func.arg_type_infos), num_caller_args)
                for i, arg_ti in enumerate(func.arg_type_infos):
                    call_arg_node = node.args[i].get()
                    call_ti = self.ast_context.get_type_info_by_node(call_arg_node)
                    self._handle_pointer(arg_ti, call_ti, call_arg_node)

            # check if we are trying to take the address of any literals
            for i, arg_node in enumerate(node.args):
                arg_node = arg_node.get()
                if nodeattrs.get_attr(arg_node, nodeattrs.ADDRESS_OF_NODE_MD):
                    if not isinstance(arg_node, ast.Name):
                        ident_node = self._add_assignment_to_tmp_ident(node, arg_node)
                        nodeattrs.set_attr(ident_node, nodeattrs.ADDRESS_OF_NODE_MD)
                        node.args[i] = ident_node

    def compare(self, node, num_children_visited):
        super().compare(node, num_children_visited)
        if num_children_visited == -1:
            # incomplete ... - just getting a test to pass for now:
            # if a == "foo" -> if *a == "foo"
            if isinstance(node.left, ast.Name):
                left_ti = self.ast_context.get_type_info_by_node(node.left)
                rhs = node.comparators[0]
                if isinstance(rhs, ast.Constant):
                    if left_ti.is_pointer:
                        rhs_ti = self.ast_context.get_type_info_by_node(rhs)
                        if not rhs_ti.is_none_type:
                            nodeattrs.set_attr(node.left, nodeattrs.DEREF_NODE_MD)

    def rtn(self, node, num_children_visited):
        super().rtn(node, num_children_visited)
        if num_children_visited == -1:
            rtn_type_info = self.ast_context.get_type_info_by_node(node)
            returned_node = node.value
            ti = self.ast_context.get_type_info_by_node(returned_node)
            self._handle_pointer(rtn_type_info, ti, returned_node)
            if nodeattrs.get_attr(returned_node, nodeattrs.ADDRESS_OF_NODE_MD):
                if not isinstance(returned_node, ast.Name):
                    # we need a name node to deref or take the address
                    # of - but this isn't specific to rtn?
                    # return "foo"
                    # ->
                    # a = "foo"
                    # return &a
                    # (this works in golang, won't work for c ...)
                    if (isinstance(returned_node, ast.Constant) and
                        returned_node.value is None):
                        # special case - return nil is fine
                        pass
                    else:
                        ident_node = self._add_assignment_to_tmp_ident(node, node.value)
                        node.value = ident_node
                        self._handle_pointer(rtn_type_info, ti, node.value)

    def assign(self, node, num_children_visited):
        super().assign(node, num_children_visited)
        if num_children_visited == -1:
            scope = self.ast_context.current_scope.get()
            lhs = node.targets[0].get()
            lhs_ti = self.ast_context.get_type_info_by_node(lhs)
            rhs = node.value
            if isinstance(rhs, ast.Call):
                func = nodeattrs.get_function(rhs)
                if func.is_builtin:
                    # this is a builtin function - for now we assume
                    # builtins don't return pointers - this is obviously
                    # wrong
                    if lhs_ti.is_pointer:
                        nodeattrs.set_attr(lhs, nodeattrs.DEREF_NODE_MD)
                else:
                    rtn_ti = func.get_rtn_type_info()
                    if lhs_ti != rtn_ti:
                        if rtn_ti.is_pointer and not lhs_ti.is_pointer:
                            # if we are changing the type to pointer, this
                            # better be a declaration node (ie not an ident
                            # being re-used)
                            assert scope.is_declaration_node(lhs)
                            lhs_ti.is_pointer = True
                        else:
                            raise AssertionError("why are these types not matching?")
            elif isinstance(rhs, ast.Subscript):
                self._handle_subscript_rhs(rhs)
                rhs_ti = self.ast_context.get_type_info_by_node(rhs)
                # why call this generic method only here?
                self._handle_pointer(lhs_ti, rhs_ti, rhs.value)
            elif isinstance(rhs, ast.Constant):
                if isinstance(lhs, ast.Name):
                    # since we are assigning a constant, we need to look at the
                    # type of the declaration node, because typevisitor just
                    # looks at the type at the rhs
                    decl_node = scope.get_declaration_node(lhs.id)
                    decl_node_ti = self.ast_context.get_type_info_by_node(decl_node)
                    if decl_node_ti.is_pointer:
                        if lhs is decl_node:
                            # we do not deref if this is the decl node
                            # because this is wrong var foo *string = "test"
                            pass
                        else:
                            nodeattrs.set_attr(lhs, nodeattrs.DEREF_NODE_MD)

    def subscript(self, node, num_children_visited):
        super().subscript(node, num_children_visited)
        if num_children_visited == -1:
            self._handle_subscript_rhs(node)

    def binop(self, node, num_children_visited):
        super().binop(node, num_children_visited)
        if num_children_visited == -1:
            # -> "hello " + *msg, not "hello " + msg
            self._deref_if_pointer(self.ast_context.get_type_info_by_node(node.left), node.left)
            self._deref_if_pointer(self.ast_context.get_type_info_by_node(node.right), node.right)

    def _add_assignment_to_tmp_ident(self, node, rhs_node):
        ident_name = self.ast_context.get_unique_identifier_name()
        ident_assignment = nodebuilder.assignment(ident_name, rhs_node)
        lhs_node = ident_assignment.targets[0]
        ti = self.ast_context.get_type_info_by_node(rhs_node)
        self.ast_context.register_type_info_by_node(lhs_node, ti)
        nodes.insert_node_above(ident_assignment, self.parent_body, node)
        if isinstance(rhs_node, ast.Subscript):
            self._handle_subscript_rhs(rhs_node)
        return copy.copy(lhs_node)

    def _handle_subscript_rhs(self, node):
        """
        TODO handle this generically with a visit method, add multiple passes
        to this visitor, run it in the last pass.
        """
        assert isinstance(node, ast.Subscript)
        if isinstance(node.value, ast.Name):
            #   val = l[0] # if l is a pointer, we need:
            # ->
            #   val = (*l)[0]
            ti = self.ast_context.get_type_info_by_node(node.value)
            if ti.is_pointer:
                nodeattrs.set_attr(node, nodeattrs.DEREF_NODE_MD)
                nodeattrs.set_attr(node.value, nodeattrs.DEREF_NODE_MD)
        if isinstance(node.slice, ast.Name):
            #   val = d[key] # if key is a pointer, we need
            # ->
            #   val = d[*key]
            ti = self.ast_context.get_type_info_by_node(node.slice)
            if ti.is_pointer:
                nodeattrs.set_attr(node.slice, nodeattrs.DEREF_NODE_MD)

    def _handle_pointer(self, required_type_info, type_info, node):
        if required_type_info.is_pointer:
            if type_info.is_none_type:
                # ok - pass null to something that takes a pointer
                pass
            elif not type_info.is_pointer:
                nodeattrs.set_attr(node, nodeattrs.ADDRESS_OF_NODE_MD)
        else:
            if type_info.is_pointer:
                nodeattrs.set_attr(node, nodeattrs.DEREF_NODE_MD)

    def _deref_if_pointer(self, type_info, node):
        if type_info.is_pointer:
            nodeattrs.set_attr(node, nodeattrs.DEREF_NODE_MD)


class WithRemover(visitor.NoopNodeVisitor):
    """
    Removes With. Have to add Try/Except.
    """
    def __init__(self, ast_context):
        super().__init__()
        self.ast_context = ast_context

    def with_resource(self, node, num_children_visited):
        super().with_resource(node, num_children_visited)
        if num_children_visited == 0:
            scope = self.ast_context.current_scope.get()
            insert_index = nodes.get_body_insert_index(scope.ast_node.body, node) + 1
            for i, item in enumerate(node.items):
                n = nodebuilder.assignment(item.optional_vars, item.context_expr)
                scope.ast_node.body.insert(insert_index + i, n)
            for i, b in enumerate(node.body):
                scope.ast_node.body.insert(insert_index + i + len(node.items), b)


class DocStringHandler(visitor.NoopNodeVisitor):
    """
    Finds doc strings, removes them from the ast and associates them with
    their method definition.
    """
    def __init__(self, ast_context):
        super().__init__()
        self.ast_context = ast_context

    def funcdef(self, node, num_children_visited):
        super().funcdef(node, num_children_visited)
        if num_children_visited == -1:
            if isinstance(node.body[0], ast.Expr):
                if isinstance(node.body[0].value, ast.Constant):
                    if isinstance(node.body[0].value.value, str):
                        func = nodeattrs.get_function(node)
                        func.docstring = node.body[0].value.value
                        del node.body[0]


class CallsiteVisitor(visitor.NoopNodeVisitor):
    """
    This visitor collects function caller information.

    If the caller assigns to a single ident, then the function returns a tuple.
    If the caller assigns to multiple values, the function can return multiple
    values. This seems like good default behavior.
    """
    def assign(self, node, num_children_visited):
        super().assign(node, num_children_visited)
        if num_children_visited == 0:
            assert len(node.targets) == 1
            lhs = node.targets[0].get()
            rhs = node.value
            if isinstance(rhs, ast.Call):
                func = nodeattrs.get_function(rhs)
                unpacking = isinstance(lhs, ast.Tuple)
                func.caller_unpacks_return_value = unpacking
                func.caller_assigns_single_return_value = not unpacking


class UnpackingRewriter(BodyParentNodeVisitor):
    """
    If the target language does not support unpacking a tuple
    (not has_assignment_lhs_unpacking):

    a, b = [1, 2]
    =>
    t0 = [1, 2]
    a = t0[0]
    b = t0[1]
    """
    def __init__(self, ast_context, target):
        super().__init__()
        self.ast_context = ast_context
        self.target = target

    def assign(self, node, num_children_visited):
        super().assign(node, num_children_visited)
        if num_children_visited == 0:
            assert len(node.targets) == 1
            lhs = node.targets[0].get()
            rhs = node.value
            rewrite = self._should_rewrite(lhs, rhs)
            if rewrite:
                if isinstance(rhs, ast.Name):
                    ident_node = rhs
                    # skip the original assignment node
                    setattr(node, nodeattrs.SKIP_NODE_ATTR, True)
                else:
                    ident_name = self.ast_context.get_unique_identifier_name()
                    ident_node = nodebuilder.identifier(ident_name)
                    setattr(lhs, nodeattrs.ALT_NODE_ATTR, ident_node)
                self._add_subscribt_assignments(node, ident_node, lhs.elts)

    def loop_for(self, node, num_children_visited, is_foreach):
        super().loop_for(node, num_children_visited, is_foreach)
        if num_children_visited == 0:
            rewrite = True
            if isinstance(node.iter, ast.Call):
                func = nodeattrs.get_function(node.iter)
                if func.name == "enumerate":
                    # enumerate in a for loop is handled by each target language
                    # impl - it would be nice to make this more explicit
                    # also, we should check for "builtin" here?
                    rewrite = False
            if rewrite:
                rewrite = self._should_rewrite(node.target, node.iter)
            if rewrite:
                ident_name = self.ast_context.get_unique_identifier_name()
                ident_node = nodebuilder.identifier(ident_name)
                for i, target_node in enumerate(node.target.elts):
                    n = nodebuilder.assignment(
                        target_node,
                        nodebuilder.subscript_list(ident_name, i))
                    node.body.insert(i, n)
                setattr(node.target, nodeattrs.ALT_NODE_ATTR, ident_node)

    def _add_subscribt_assignments(self, node, list_ident_node, target_nodes):
        insert_index = nodes.get_body_insert_index(self.parent_body, node) + 1
        varname = list_ident_node.id
        for i, target_node in enumerate(target_nodes):
            if target_node.id == "_":
                # by convention, "_" is the "throwaway identifier" in Python
                # for example: first_name, _ = full_name.split(" ")
                # we are skipping it when rewriting unpacking as individual
                # assignments
                continue
            n = nodebuilder.assignment(
                target_node,
                nodebuilder.subscript_list(varname, i))
            self.parent_body.insert(insert_index, n)
            insert_index += 1

    def _should_rewrite(self, lhs, rhs):
        rewrite = isinstance(lhs, ast.Tuple)
        if isinstance(rhs, ast.Call):
            func = nodeattrs.get_function(rhs)
            # this isn't right when this rewrite happens for a for-loop
            # the return type of the function is not what the iteration
            # variable actually gets - it gets the contained type instead
            if func.returns_multiple_values(self.target):
                rewrite = False
            if self.target.has_assignment_lhs_unpacking:
                rewrite = False
        else:
            if self.target.has_assignment_lhs_unpacking:
                rewrite = False
        return rewrite
            

class IdentifierCollector(visitor.NoopNodeVisitor):
    """
    Collects all identifier names, so that when new identifier names are
    generated, we can avoid clashes.
    """
    def __init__(self, ast_context):
        super().__init__()
        self.ast_context = ast_context
        self.ident_names = set()

    def on_scope_released(self, scope):
        super().on_scope_released(scope)
        self.ident_names.update(scope.get_identifiers_in_this_scope())

    def sayonara(self):
        super().sayonara()
        self.ast_context.register_ident_names(self.ident_names)


class LambdaReturnVisitor(visitor.NoopNodeVisitor):
    """
    Adds return stmt to lambda function bodies unless "explicit_rtn" is False or
    unless this is Python. (return stmt removal is done in tokenvisitor for
    simplicity).
    """
    def __init__(self, ast_context, target):
        super().__init__()
        self.ast_context = ast_context
        self.target = target

    def lambdadef(self, node, num_children_visited):
        super().lambdadef(node, num_children_visited)
        if num_children_visited == -1:
            if self.target.explicit_rtn and not targets.is_python(self.target):
                last_node = node.body[-1].get()
                ti = self.ast_context.get_type_info_by_node(last_node)
                if ti.is_real:
                    rtn_node = nodebuilder.rtn(nodes.shallow_copy_node(last_node, self.ast_context))
                    nodeattrs.set_attr(last_node, nodeattrs.ALT_NODE_ATTR, rtn_node)


class ReturnValueMapper(BodyParentNodeVisitor):
    """
    The difficulty about transcompiling is the potential abstraction mismatch
    between the source and the target language.
    In its simplest form, this mismatch manifests itself as different "default"
    or "marker" return values. For example the Python index method on strings
    returns -1 if a substring isn't found. In Elisp, a similar function,
    cl-search, returns nil if the substring isn't found.

    There are multiple possible strategies to address this problem:
      - Wrapper functions, ie wrap cl-search with a custom function that returns
        -1 instead of nil when the substring isn't found
      - Track how the return value is used and adjust comparisions of necessary
        so look for all "i == -1" checks and change them to "i is None".
      - Right after calling the function, insert an if stmt that maps -1 to None
    
    This visitor implements the last approach.
    """

    MAPPED_RTN_VALUE_OLD_VALUE_ATTR = "__rtn_value_mapping_old_value"
    MAPPED_RTN_VALUE_NEW_VALUE_ATTR = "__rtn_value_mapping_new_value"
    
    def __init__(self, ast_context):
        super().__init__()
        self.ast_context = ast_context

    def call(self, node, num_children_visited):
        super().call(node, num_children_visited)
        if num_children_visited == -1:
            assign_nodes = nodes.extract_expressions_with_attr(
                node, self.parent_body,
                ReturnValueMapper.MAPPED_RTN_VALUE_OLD_VALUE_ATTR,
                self.ast_context,
                ignore_start_node=True,
                tmp_ident_prefix="i") # TODO
            for n in assign_nodes:
                lhs = nodes.get_assignment_lhs(n)
                rhs = nodes.get_assignment_rhs(n)
                old_value = nodeattrs.get_attr(
                    rhs, ReturnValueMapper.MAPPED_RTN_VALUE_OLD_VALUE_ATTR,
                    remove_attr=True, must_exist=True)
                new_value = nodeattrs.get_attr(
                    rhs, ReturnValueMapper.MAPPED_RTN_VALUE_NEW_VALUE_ATTR,
                    remove_attr=True, must_exist=True)
                if_stmt = nodebuilder.if_stmt(
                    nodebuilder.compare(lhs.id, "==", nodebuilder.constant(old_value)),
                    body=nodebuilder.assignment(lhs.id, nodebuilder.constant(new_value)))
                nodes.insert_node_below(
                    if_stmt, self.parent_body, n)


class ImportVisitor(visitor.NoopNodeVisitor):
    """
    This visitor adds import statements for type mappings. It also removes
    existing (Python) import statements.

    Imports for function rewrite rules are handled when they are applied.
    """
    def __init__(self, ast_context, target):
        super().__init__()
        self.ast_context = ast_context
        self.target = target

    def name(self, node, num_children_visited):
        super().name(node, num_children_visited)
        if num_children_visited == 0:
            scope = self.ast_context.current_scope.get()
            if scope.is_declaration_node(node):
                ti = self.ast_context.lookup_type_info_by_node(node)
                type_mapping = self.target.type_mapper.get_type_mapping(ti)
                self.ast_context.register_imports(type_mapping.imports)

    def module(self, node, num_children_visited):
        super().module(node, num_children_visited)
        if num_children_visited == -1:
            # this works because at this point all module child nodes have been
            # visited and imports have been registered
            for i in reversed(self.ast_context.get_imports()):
                node.body.insert(0, nodebuilder.import_node(i))

    def import_stmt(self, node, num_children_visited):
        super().import_stmt(node, num_children_visited)
        if num_children_visited == 0:
            # gets rid of existing imports
            nodeattrs.skip(node)


class SelflessVisitor(visitor.NoopNodeVisitor):
    """
    Removes the implicit "self" argument in class methods.
    Flying back from London after House Boat Geese.
    """
    def __init__(self, ast_context):
        super().__init__()
        self.ast_context = ast_context
        self.current_class = None
        self.current_method = None

    def funcarg(self, node, num_children_visited):
        super().funcarg(node, num_children_visited)
        scope = self.ast_context.current_scope.get()
        class_name, _ = scope.get_enclosing_class()
        if class_name is not None:
            if self.current_class != class_name:
                self.current_method = None
            self.current_class = class_name
            method, _ = scope.get_enclosing_namespace()
            assert method is not None
            if self.current_method != method:
                nodeattrs.skip(node)
                self.current_method = method


class MemberVariableVisitor(visitor.NoopNodeVisitor):
    """
    Adds explicit member variables.
    """
    def __init__(self, ast_context):
        super().__init__()
        self.ast_context = ast_context

    def classdef(self, node, num_children_visited):
        super().classdef(node, num_children_visited)
        if num_children_visited == -1:
            res = self.ast_context.resolver
            class_type = tim.TypeInfo.clazz(node.name)
            for name, ti in res.get_all_attributes_name_and_type(class_type):
                decl_node = nodebuilder.assignment(name, None)
                nodeattrs.set_type_info(decl_node.targets[0], ti)
                # self.foo = 3 -> int foo member variable
                node.body.insert(0, decl_node)


class RenameSelfReceiverVisitor(visitor.NoopNodeVisitor):
    """
    Rename self. references within class definitions.
    """
    def __init__(self, ast_context, self_receiver_name):
        super().__init__()
        self.ast_context = ast_context
        self.self_receiver_name = self_receiver_name

    def name(self, node, num_children_visited):
        super().name(node, num_children_visited)
        scope = self.ast_context.current_scope.get()
        class_name, _ = scope.get_enclosing_class()
        if class_name is not None:
            if node.id == "self":
                node.id = self.self_receiver_name


class LameSemanticCheckerVisitor(_CommonStateVisitor):
    """
    Detects simple errors in the AST.
    """
    def name(self, node, num_children_visited):
        super().name(node, num_children_visited)
        if self.visiting_func:
            pass
        elif self.assign_visiting_lhs:
            pass
        elif self.loop_visiting_lhs:
            pass
        else:
            scope = self.ast_context.current_scope.get()
            decl_node = scope.get_declaration_node(node.id)
            assert decl_node is not None, "Unknown identifier [%s]" % node.id


class NodeCollectingVisitor(visitor.NoopNodeVisitor):
    
    def __init__(self, condition_callback):
        super().__init__()
        self.condition_callback = condition_callback
        self.nodes = []

    def generic_visit(self, node, num_children_visited):
        super().generic_visit(node, num_children_visited)
        if num_children_visited == -1:
            if self.condition_callback(node):
                self.nodes.append(node)
