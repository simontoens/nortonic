import ast
import visitor.nodeattrs as nodeattrs


class NoopNodeVisitor:
    """
    Abstract base class that contains optional methods for visitors to
    implement.
    """

    def __init__(self, delegate=None):
        # if not None, visitor method calls are made on this delegate
        self._delegate = delegate


    #
    # Optional customization of re-visiting behavior
    #

    @property
    def leave_early(self):
        """
        If this property is True, visiting stops and should_revisit is called.
        """
        if self._delegate is None:
            return False
        else:
            return self._delegate.leave_early

    @property
    def should_revisit(self):
        """
        Called when all nodes of the AST have been visited or when leave_early
        is True. Returns True if the visitation should start over from the
        beginning, or False if once was enough.

        The default is False.
        """
        if self._delegate is None:
            return False
        else:
            return self._delegate.should_revisit


    #
    # Optional lifecycle callbacks
    #

    def visited(self):
        """
        Given a call to visitor.visit(node, visitor), this method is called
        on the given visitor instance after a "completed visit".
        By default there is only one visit (so this method would only get
        callled once in that case), but based on how "should_revisit" is
        implemented, there can be multiple visits, so therefore this method
        would also get called more than once in that case.
        """
        if self._delegate is not None:
            self._delegate.visited()

    def sayonara(self):
        """
        Given a call to visitor.visit(node, visitor), this is the very last
        method called on the given visitor instance before the call to the
        visit function returns. This method is only called once.
        """
        if self._delegate is not None:
            self._delegate.sayonara()


    #
    # Optional scope lifecyle callbacks
    #

    def on_scope_pushed(self, scope):
        if self._delegate is not None:
            self._delegate.on_scope_pushed(scope)

    def on_scope_released(self, scope):
        if self._delegate is not None:
            self._delegate.on_scope_released(scope)


    #
    # Typed visiting methods - one per AST node type
    #

    def boolop_and(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.boolop_and(node, num_children_visited)

    def boolop_or(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.boolop_or(node, num_children_visited)
        
    def add(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.add(node, num_children_visited)

    def uadd(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.uadd(node, num_children_visited)

    def sub(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.sub(node, num_children_visited)

    def usub(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.usub(node, num_children_visited)

    def div(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.div(node, num_children_visited)
            
    def mult(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.mult(node, num_children_visited)

    def mod(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.mod(node, num_children_visited)

    def attr(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.attr(node, num_children_visited)

    def unaryop(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.unaryop(node, num_children_visited)

    def binop(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.binop(node, num_children_visited)

    def boolop(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.boolop(node, num_children_visited)

    def assign(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.assign(node, num_children_visited)

    def assign_aug(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.assign_aug(node, num_children_visited)

    def call(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.call(node, num_children_visited)

    def compare(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.compare(node, num_children_visited)

    def cond_if(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.cond_if(node, num_children_visited)

    def cond_else(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.cond_else(node, num_children_visited)

    def cond_if_expr(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.cond_if_expr(node, num_children_visited)

    def cond_if_expr_else(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.cond_if_expr_else(node, num_children_visited)

    def constant(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.constant(node, num_children_visited)

    def eq(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.eq(node, num_children_visited)

    def not_eq(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.not_eq(node, num_children_visited)

    def unary_not(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.unary_not(node, num_children_visited)

    def less_than(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.less_than(node, num_children_visited)

    def greater_than(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.greater_than(node, num_children_visited)

    def same(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.same(node, num_children_visited)

    def not_same(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.not_same(node, num_children_visited)

    def expr(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.expr(node, num_children_visited)

    def funcarg(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.funcarg(node, num_children_visited)

    def funcdef(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.funcdef(node, num_children_visited)

    def classdef(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.classdef(node, num_children_visited)

    def lambdadef(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.lambdadef(node, num_children_visited)

    def loop_for(self, node, num_children_visited, is_foreach):
        if self._delegate is not None:
            self._delegate.loop_for(node, num_children_visited, is_foreach)

    def loop_continue(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.loop_continue(node, num_children_visited)

    def loop_break(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.loop_break(node, num_children_visited)

    def container_type_dict(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.container_type_dict(node, num_children_visited)
            
    def container_type_list(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.container_type_list(node, num_children_visited)

    def container_type_tuple(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.container_type_tuple(node, num_children_visited)

    def imports(self, node, num_children_visited):
        """
        Pseudo-visitor method not corresponding to a real node in the ast.
        Marks the beginning and the end of import statements.
        """
        if self._delegate is not None:
            self._delegate.imports(node, num_children_visited)

    def import_stmt(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.import_stmt(node, num_children_visited)

    def module(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.module(node, num_children_visited)

    def name(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.name(node, num_children_visited)

    def pass_stmt(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.pass_stmt(node, num_children_visited)

    def rtn(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.rtn(node, num_children_visited)

    def slice(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.slice(node, num_children_visited)
            
    def subscript(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.subscript(node, num_children_visited)

    def block(self, node, num_children_visited, is_root_block, body):
        if self._delegate is not None:
            self._delegate.block(node, num_children_visited, is_root_block, body)

    def stmt(self, node, num_children_visited, parent_node, is_last_body_stmt):
        if self._delegate is not None:
            self._delegate.stmt(node, num_children_visited, parent_node, is_last_body_stmt)

    def with_resource(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.with_resource(node, num_children_visited)

    def list_comp(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.list_comp(node, num_children_visited)

    def list_comp_generator(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.list_comp_generator(node, num_children_visited)

    def generic_visit(self, node, num_children_visited):
        if self._delegate is not None:
            self._delegate.generic_visit(node, num_children_visited)


def visit(root, visitor, verbose=False, skip_skipped_nodes=True):
    if verbose:
        print("START", visitor)
    if verbose:
        print("INITIAL VISIT", visitor)
    _visit(root, visitor, verbose, skip_skipped_nodes)
    visitor.visited()
    while visitor.should_revisit:
        if verbose:
            print("RE-VISIT", visitor)
        _visit(root, visitor, verbose, skip_skipped_nodes)
        visitor.visited()
    visitor.sayonara()
    if verbose:
        print("END", visitor)


def _visit(node, visitor, verbose, skip_skipped_nodes=True):
    if visitor.leave_early:
        return

    # handle special 'alt' attribute, which points to an alternative node
    # to look at instead - see astrewriter.py
    if hasattr(node, nodeattrs.ALT_NODE_ATTR):
        alt_node = getattr(node, nodeattrs.ALT_NODE_ATTR)
        if verbose:
            print("_visiting alt node", alt_node, "instead of", node)
        _visit(alt_node, visitor, verbose)
    elif skip_skipped_nodes and nodeattrs.is_skipped(node):
        pass
    else:
        if verbose:
            print("_visit node start", node)

        visitor.generic_visit(node, 0)
            
        if isinstance(node, ast.arg):
            visitor.funcarg(node, 0)
        elif isinstance(node, ast.UnaryOp):
            visitor.unaryop(node, 0)
            _visit(node.op, visitor, verbose)
            visitor.unaryop(node, 1)
            _visit(node.operand, visitor, verbose)
            visitor.unaryop(node, -1)
        elif isinstance(node, ast.And):
            visitor.boolop_and(node, 0)
        elif isinstance(node, ast.Or):
            visitor.boolop_or(node, 0)
        elif isinstance(node, ast.Add):
            visitor.add(node, 0)
        elif isinstance(node, ast.UAdd):
            visitor.uadd(node, 0)
        elif isinstance(node, ast.Sub):
            visitor.sub(node, 0)
        elif isinstance(node, ast.USub):
            visitor.usub(node, 0)                   
        elif isinstance(node, ast.Div):
            visitor.div(node, 0)
        elif isinstance(node, ast.Mult):
            visitor.mult(node, 0)
        elif isinstance(node, ast.Mod):
            visitor.mod(node, 0)
        elif isinstance(node, ast.BinOp):
            visitor.binop(node, 0)
            _visit(node.left, visitor, verbose)
            visitor.binop(node, 1)
            _visit(node.op, visitor, verbose)
            visitor.binop(node, 2)
            _visit(node.right, visitor, verbose)
            visitor.binop(node, -1)
        elif isinstance(node, ast.BoolOp):
            visitor.boolop(node, 0)
            for i, value in enumerate(node.values):
                _visit(value, visitor, verbose)
                if i < len(node.values) - 1:
                    _visit(node.op, visitor, verbose)
            visitor.boolop(node, -1)
        elif isinstance(node, ast.Assign):
            assert len(node.targets) == 1
            visitor.assign(node, 0)
            _visit(node.targets[0], visitor, verbose)
            visitor.assign(node, 1)
            _visit(node.value, visitor, verbose)
            visitor.assign(node, -1)
        elif isinstance(node, ast.AugAssign): # a += 1
            visitor.assign_aug(node, 0)
            _visit(node.target, visitor, verbose)
            visitor.assign_aug(node, 1)
            _visit(node.op, visitor, verbose)
            visitor.assign_aug(node, 2)
            _visit(node.value, visitor, verbose)
            visitor.assign_aug(node, -1)
        elif isinstance(node, ast.Attribute):
            visitor.attr(node, 0)
            _visit(node.value, visitor, verbose)
            # ast.Attribute also has a 'ctx' attr, which is of type ast.Load
            visitor.attr(node, -1)
        elif isinstance(node, ast.Call):
            visitor.call(node, 0)
            _visit(node.func, visitor, verbose)
            visitor.call(node, 1)
            for i, arg in enumerate(node.args):
                _visit(arg, visitor, verbose)
                visitor.call(node, i+2)
            if hasattr(node, "body"):
                _visit_body_statements(node, node.body, visitor, is_root_block=False, verbose=verbose)
            assert len(node.keywords) == 0, "function argument keywords are not handled yet"
            visitor.call(node, -1)
        elif isinstance(node, ast.Constant):
            visitor.constant(node, 0)
        elif isinstance(node, ast.Continue):
            visitor.loop_continue(node, 0)
        elif isinstance(node, ast.Break):
            visitor.loop_break(node, 0)
        elif isinstance(node, ast.Compare):
            visitor.compare(node, 0)
            _visit(node.left, visitor, verbose)
            assert len(node.ops) == 1
            visitor.compare(node, 1)
            _visit(node.ops[0], visitor, verbose)
            assert len(node.comparators) == 1
            visitor.compare(node, 2)
            _visit(node.comparators[0], visitor, verbose)
            visitor.compare(node, -1)
        elif isinstance(node, ast.Eq):
            visitor.eq(node, 0)
        elif isinstance(node, ast.NotEq):
            visitor.not_eq(node, 0)
        elif isinstance(node, ast.Not):
            visitor.unary_not(node, 0)
        elif isinstance(node, ast.Is):
            visitor.same(node, 0)
        elif isinstance(node, ast.IsNot):
            visitor.not_same(node, 0)            
        elif isinstance(node, ast.Lt):
            visitor.less_than(node, 0)
        elif isinstance(node, ast.Gt):
            visitor.greater_than(node, 0)
        elif isinstance(node, ast.ClassDef):
            visitor.classdef(node, 0)
            _visit_body_statements(node, node.body, visitor, is_root_block=False, verbose=verbose)
            visitor.classdef(node, -1)
        elif isinstance(node, ast.FunctionDef):
            visitor.funcdef(node, 0)
            for a in node.args.args:
                _visit(a, visitor, verbose)
            visitor.funcdef(node, len(node.args.args) + 1)
            _visit_body_statements(node, node.body, visitor, is_root_block=False, verbose=verbose)
            visitor.funcdef(node, -1)
        elif isinstance(node, ast.Lambda):
            visitor.lambdadef(node, 0)
            for a in node.args.args:
                _visit(a, visitor, verbose)
            visitor.lambdadef(node, len(node.args.args) + 1)
            if isinstance(node.body, ast.AST):
                # in python the body of a lambda is a singe ast node but
                # we are a bit more flexible
                node.body = [node.body]
            _visit_body_statements(node, node.body, visitor, is_root_block=False, verbose=verbose)            
            visitor.lambdadef(node, -1)
        elif isinstance(node, ast.If):
            visitor.cond_if(node, 0)
            _visit(node.test, visitor, verbose)
            visitor.cond_if(node, 1)
            _visit_body_statements(node, node.body, visitor, is_root_block=False, verbose=verbose)
            visitor.cond_if(node, -1)
            if len(node.orelse) > 0:
                visitor.cond_else(node, 0)
                _visit_body_statements(node, node.orelse, visitor, is_root_block=False, verbose=verbose)
                visitor.cond_else(node, -1)
        elif isinstance(node, ast.IfExp):
            visitor.cond_if_expr(node, 0)
            _visit(node.body, visitor, verbose)
            visitor.cond_if_expr(node, 1)
            _visit(node.test, visitor, verbose)
            visitor.cond_if_expr(node, 2)
            visitor.cond_if_expr_else(node, 0)
            _visit(node.orelse, visitor, verbose)
            visitor.cond_if_expr_else(node, -1)
            visitor.cond_if_expr(node, -1)
        elif isinstance(node, ast.For):
            is_foreach = not hasattr(node, nodeattrs.FOR_LOOP_C_STYLE_INIT_NODE)
            visitor.loop_for(node, 0, is_foreach=is_foreach)
            if is_foreach:
                _visit(node.target, visitor, verbose)
                visitor.loop_for(node, 1, is_foreach=True)
                _visit(node.iter, visitor, verbose)
                visitor.loop_for(node, 2, is_foreach=True)
            else:
                _visit(getattr(node, nodeattrs.FOR_LOOP_C_STYLE_INIT_NODE), visitor, verbose)
                visitor.loop_for(node, 1, is_foreach=False)
                _visit(getattr(node, nodeattrs.FOR_LOOP_C_STYLE_COND_NODE), visitor, verbose)
                visitor.loop_for(node, 2, is_foreach=False)
                _visit(getattr(node, nodeattrs.FOR_LOOP_C_STYLE_EXPR_NODE), visitor, verbose)
                visitor.loop_for(node, 3, is_foreach=False)
                
            _visit_body_statements(node, node.body, visitor, is_root_block=False, verbose=verbose)
            if len(node.orelse) > 0:
                visitor.cond_else(node, 0)
                _visit_body_statements(node, node.orelse, visitor, is_root_block=False, verbose=verbose)
                visitor.cond_else(node, -1)
            visitor.loop_for(node, -1, is_foreach=is_foreach)
        elif isinstance(node, ast.Dict):
            visitor.container_type_dict(node, 0)
            assert len(node.keys) == len(node.values)
            num_children_visited = 1
            for i in range(0, len(node.keys)):
                _visit(node.keys[i], visitor, verbose)
                visitor.container_type_dict(node, num_children_visited)
                num_children_visited += 1
                _visit(node.values[i], visitor, verbose)
                visitor.container_type_dict(node, num_children_visited)
                num_children_visited += 1
            visitor.container_type_dict(node, -1)
        elif isinstance(node, ast.List):
            visitor.container_type_list(node, 0)
            for i, n in enumerate(node.elts):
                _visit(n, visitor, verbose)
                visitor.container_type_list(node, i+1)
            visitor.container_type_list(node, -1)
        elif isinstance(node, ast.Pass):
            visitor.pass_stmt(node, -1)
        elif isinstance(node, ast.Slice):
            assert node.step is None
            visitor.slice(node, 0)
            if node.lower is not None:
                _visit(node.lower, visitor, verbose)
            visitor.slice(node, 1)
            if node.upper is not None:
                _visit(node.upper, visitor, verbose)
            visitor.slice(node, -1)
        elif isinstance(node, ast.Subscript):
            visitor.subscript(node, 0)
            _visit(node.value, visitor, verbose)
            visitor.subscript(node, 1)
            _visit(node.slice, visitor, verbose)
            visitor.subscript(node, -1)
        elif isinstance(node, ast.Tuple):
            visitor.container_type_tuple(node, 0)
            for i, n in enumerate(node.elts):
                _visit(n, visitor, verbose)
                visitor.container_type_tuple(node, i+1)
            visitor.container_type_tuple(node, -1)
        elif isinstance(node, ast.Module):
            visitor.module(node, 0)
            import_nodes = [n for n in node.body if isinstance(n, ast.Import) and not nodeattrs.is_skipped(n)]
            if len(import_nodes) > 0:
                visitor.imports(None, 0) # which node to pass in?
                _visit_body_statements(node, import_nodes, visitor,
                                       is_root_block=True,
                                       start_at_node=0,
                                       verbose=verbose)
                visitor.imports(None, -1)
            _visit_body_statements(node, node.body, visitor,
                                   is_root_block=True,
                                   start_at_node=len(import_nodes),
                                   verbose=verbose)
            visitor.module(node, -1)
        elif isinstance(node, ast.Name):
            visitor.name(node, 0)
        elif isinstance(node, ast.Expr):
            visitor.expr(node, 0)
            _visit(node.value, visitor, verbose)
            visitor.expr(node, -1)
        elif isinstance(node, ast.Return):
            visitor.rtn(node, 0)
            _visit(node.value, visitor, verbose)
            visitor.rtn(node, -1)        
        elif isinstance(node, ast.Import):
            visitor.import_stmt(node, 0)
        elif isinstance(node, ast.With):
            visitor.with_resource(node, 0)
        elif isinstance(node, ast.ListComp):
            visitor.list_comp(node, 0)
            _visit(node.elt, visitor, verbose)
            visitor.list_comp(node, 1)
            assert len(node.generators) == 1 # TODO
            gen = node.generators[0]
            visitor.list_comp_generator(gen, 0)
            _visit(gen.target, visitor, verbose)
            visitor.list_comp_generator(gen, 1)
            _visit(gen.iter, visitor, verbose)
            visitor.list_comp_generator(gen, 2)
            if len(gen.ifs) > 0:
                assert len(gen.ifs) == 1 # TODO
                _visit(gen.ifs[0], visitor, verbose)
                visitor.list_comp_generator(gen, 3)
            visitor.list_comp_generator(gen, -1)
            visitor.list_comp(node, -1)
        else:
            assert False, "Unknown node %s" % node

        visitor.generic_visit(node, -1)


def _visit_body_statements(node, body, visitor, is_root_block, start_at_node=0, verbose=False):
    visitor.block(node, 0, is_root_block, body)
    body_copy = list(body)
    for i in range(start_at_node, len(body_copy)):
        child_node = body_copy[i].get()
        is_last_body_stmt = i == len(body_copy) - 1
        visitor.stmt(child_node, 0, node, is_last_body_stmt)
        _visit(child_node, visitor, verbose)
        visitor.stmt(child_node, -1, node, is_last_body_stmt)
    visitor.block(node, -1, is_root_block, body)


def nstr(node):
    """
    See:
    print(ast.dump(ast.parse("not True"), indent=2))
Module(
  body=[
    Expr(
      value=UnaryOp(
        op=Not(),
        operand=Constant(value=True)))],
  type_ignores=[])
    """
    if isinstance(node, ast.Assign):
        return "[assign %s]" % node.targets[0].id
    if isinstance(node, ast.Attribute):
        return "[attr %s]" % node.attr
    elif isinstance(node, ast.Call):
        if isinstance(node.func, ast.Name):
            func_info = node.func.id
        elif isinstance(node.func, ast.Attribute):
            func_info = "attr %s" % node.func.attr
        else:
            func_info = "???"
        return "[call %s]" % func_info
    elif isinstance(node, ast.Expr):
        return "[expr %s]" % nstr(node.value)
    elif isinstance(node, ast.Name):
        return "[name %s]" % node.id
    else:
        return "[unknown node %s]" % node
