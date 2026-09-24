import api.bonsaibuilder as bonsaibuilder
import ast
import lang.astrewriter as astrewriter
import lang.nodes as nodes
import lang.target.rewrite as rewrite
import lang.target.targetlanguage as targetlanguage
import types
import visitor.nodeattrs as nodeattrs
import visitor.visitors as visitors


class ASTRewriterVisitor(visitors._CommonStateVisitor, visitors.BodyParentNodeVisitor):
    """
    Executes rewrite rules on the AST.
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
        rewrite_rule = self._lookup_rewrite_rule(func_name, target_type, node)
        if rewrite_rule is not None:
            assert isinstance(rewrite_rule, targetlanguage.RewriteRule)
            self.ast_context.register_imports(rewrite_rule.imports)
            args = []
            for arg_node in arg_nodes:
                type_info = self.ast_context.lookup_type_info_by_node(arg_node)
                assert type_info is not None, "unable to lookup type info for function %s: arg %s" % (func_name, arg_node)
                args.append(targetlanguage.Argument(arg_node, type_info.value_type))

            if isinstance(rewrite_rule, targetlanguage.NewRewriteRule):
                bb = (bonsaibuilder.BonsaiBuilder()
                      .set_refactor_recepe_decorator_factory(
                          lambda d: _RefactorTypeHandler(d, self.ast_context))
                      .with_node(node))
                if rewrite_rule.function_rewrite is None:
                    assert rewrite_rule.target_name is not None
                    bb.rename(rewrite_rule.target_name)
                else:
                    rewrite_rule.function_rewrite(bb)
            else:
                rw = astrewriter.ASTRewriter(node,
                                             arg_nodes,
                                             self.ast_context,
                                             self.parent_body,
                                             target_node)

                # the actual AST rewriting happens here:
                if rewrite_rule.target_name is not None:
                    rw.rename(rewrite_rule.target_name)
                if rewrite_rule.function_rewrite is not None:
                    rewrite_rule.function_rewrite(args, rw)
            self._keep_revisiting = True
            setattr(node, nodeattrs.REWRITTEN_NODE_ATTR, True)

    def _lookup_rewrite_rule(self, func_name, target_type, node):
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

    
class _RefactorTypeHandler(bonsaibuilder.BonsaiBuilder.RefactorRecepe):
    """
    Decorates the ast rewrites - adds type handling.
    """

    def __init__(self, delegate, ast_context):
        self._delegate = delegate
        self._ast_context = ast_context

    def rename(self, new_name):
        self._delegate.rename(new_name)
        ti = self._ast_context.get_type_info_by_node(self._delegate.node)
        nodeattrs.set_type_info(self._delegate.node, ti)
