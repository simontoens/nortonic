import ast as astm
import lang
import lang.internal.function as function
import lang.internal.typeinfo as ti
import visitor.asttoken as asttoken
import visitor.attrresolver as resolverm
import visitor.context as context
import visitor.nodeattrs as nodeattrs
import visitor.scopedecorator
import visitor.tokenvisitor as tokenvisitor
import visitor.typevisitor as typevisitor
import visitor.visitor as v
import visitor.visitors as  visitors


def transcompile(code, target, verbose=False):
    ctx = _init()
    root_node = astm.parse(code)
    _check_for_obvious_errors(root_node, ctx, verbose)
    _compilation_pipeline(root_node, ctx, target, verbose)
    _post_process(root_node, ctx, target, verbose)
    return _emit(root_node, ctx, target)


def _check_for_obvious_errors(root_node, ast_context, verbose=False):
    pys = lang.target.python.PythonSyntax()
    lame_checker = visitors.LameSemanticCheckerVisitor(ast_context, pys)
    v.visit(root_node, _add_scope_decorator(lame_checker, ast_context, pys), verbose)


def _compilation_pipeline(root_node, ast_context, target, verbose=False):
    """
    The main work happens here.
    """
    # determine the name of all used identifiers
    ident_collector = visitors.IdentifierCollector(ast_context)
    v.visit(root_node, _add_scope_decorator(ident_collector, ast_context, target), verbose)

    # for now, we remove with/try/except
    remover = visitors.WithRemover(ast_context)
    v.visit(root_node, _add_scope_decorator(remover, ast_context, target), verbose)

    if not lang.target.targets.is_python(target):
        # remove self arg from methods        
        selfless = visitors.SelflessVisitor(ast_context)
        v.visit(root_node, _add_scope_decorator(selfless, ast_context, target), verbose)

    _run_block_scope_puller(root_node, ast_context, target, verbose)

    # needs to run before type visitor runs for the first time
    v.visit(root_node, visitors.ContainerTypeVisitor(), verbose)
    # can we run type visitor once after block scope and unpacking?    
    _run_type_visitor(root_node, ast_context, target, verbose)

    # requires: type visitor
    # required by: unpacking rewriter visitor
    v.visit(root_node, visitors.CallsiteVisitor(), verbose)

    unpacking_rewriter = visitors.UnpackingRewriter(ast_context, target)
    v.visit(root_node, _add_scope_decorator(unpacking_rewriter, ast_context, target), verbose)

    # unpacking creates new ast nodes, they need to get associated types
    _run_type_visitor(root_node, ast_context, target, verbose)

    if not target.has_if_expr and not target.ternary_replaces_if_expr:
        # review why this rewrite rule cannot move up
        v.visit(root_node, visitors.IfExprRewriter(ast_context), verbose)
        _run_block_scope_puller(root_node, ast_context, target, verbose)
        _run_type_visitor(root_node, ast_context, target, verbose)

    if not lang.target.targets.is_python(target):
        v.visit(root_node, visitors.ListCompRewriter(ast_context), verbose)
        v.visit(root_node, visitors.ContainerTypeVisitor(), verbose)
        _run_type_visitor(root_node, ast_context, target, verbose)

    func_call_visitor = visitors.FuncCallVisitor(ast_context, target)
    v.visit(root_node, _add_scope_decorator(func_call_visitor, ast_context, target), verbose)

    rtn_values_updater = visitors.ReturnValueMapper(ast_context)
    v.visit(root_node, rtn_values_updater, verbose)

    _run_type_visitor(root_node, ast_context, target, verbose)

    # needs to re-run after ReturnValueMapper because ReturnValueMapper adds
    # new nodes that may have to be translated (for ex assignment -> call for
    # elisp) - ReturnValueMapper cannot run before the first FuncCallVisitor
    # runs: FuncCallVisitor runs the rewrites that add inputs for
    # ReturnValueMapper (old and new values)
    func_call_visitor = visitors.FuncCallVisitor(ast_context, target)
    v.visit(root_node, _add_scope_decorator(func_call_visitor, ast_context, target), verbose)

    if target.has_pointers:
        # this has to run after FuncCallVisitor because FuncCallVisitor may
        # add new assignments
        pointer_visitor = visitors.PointerVisitor(ast_context, target.pointer_types)
        v.visit(root_node, pointer_visitor, verbose)

        # we have to relax type checking a bit here because this typevisitor
        # pass will potentially change function signature arguments to pointers
        # but callsite types are left alone
        # see test_go.py
        ti.TypeInfo.TYPE_EQUALITY_CHECK_INCLUDES_POINTERS = False
        _run_type_visitor(root_node, ast_context, target, verbose)

        # this visitor marks all nodes that require "address of" or pointer
        # dereferencing, based on the fact that function argument types
        # are now pointers
        pointer_handler_visitor = visitors.PointerHandlerVisitor(ast_context)
        v.visit(root_node, _add_scope_decorator(pointer_handler_visitor, ast_context, target), verbose)
        # now that the visitor above added metadata for function callsites
        # we can re-enable stricter type checking
        ti.TypeInfo.TYPE_EQUALITY_CHECK_INCLUDES_POINTERS = True

    if not target.dynamically_typed:
        # creates member variable declarations
        # requires: type visitor
        v.visit(root_node, visitors.MemberVariableVisitor(ast_context), verbose)
        
    _run_type_visitor(root_node, ast_context, target, verbose)

    # requires: type visitor
    # required by: token visitor
    v.visit(root_node, visitors.CallsiteVisitor(), verbose)

    v.visit(root_node, visitors.LambdaReturnVisitor(ast_context, target), verbose)
    v.visit(root_node, visitors.DocStringHandler(ast_context), verbose)


def _post_process(root_node, ast_context, target, verbose=False):
    """
    Applies custom visitors and destructive visitors.
    """
    _run_optional_visitors(target.visitors, root_node, ast_context, target, verbose, with_scope=True)

    _run_type_visitor(root_node, ast_context, target, verbose)

    # requires: type visitor
    # required by: token visitor
    v.visit(root_node, visitors.CallsiteVisitor(), verbose)


    # DESTRUCTIVE AST VISITORS BELOW THIS LINE
    # ----------------------------------------
    # these visitors are "destructive" in the sense that they make changes
    # to the ast in such a way that the type visitor gets confused and cannot
    # re-create types
    # this is unfortunate and we are trying hard to avoid ast modifications
    # that make it impossible to then further re-process the ast

    if target.ternary_replaces_if_expr:
        # has to run late because it changes the ast in such a way that the
        # type visitor gets confused
        v.visit(root_node, visitors.IfExprToTernaryRewriter(), verbose)

    if target.class_self_receiver_name is not None:
        # replaces self references - this breaks type resolution currently
        # because we only add "self" to the scope
        rsrv = visitors.RenameSelfReceiverVisitor(
            ast_context, target.class_self_receiver_name)
        v.visit(root_node, _add_scope_decorator(rsrv, ast_context, target), verbose)

    # removes py import statements and adds new ones that won't make sense to
    # the type visitor
    iv = visitors.ImportVisitor(ast_context, target)
    v.visit(root_node, _add_scope_decorator(iv, ast_context, target), verbose)

    _run_optional_visitors(target.destructive_visitors, root_node, ast_context, target, verbose, with_scope=False)


def _run_optional_visitors(visitors, root_node, ast_context, target, verbose, with_scope):
    for vis in visitors:
        if hasattr(vis, "context"):
            # if this visitor has a context field, it wants the context!
            assert vis.context is None
            vis.context = ast_context
        if with_scope:
            vis = _add_scope_decorator(vis, ast_context, target)
        v.visit(root_node, vis, verbose)
    

def _run_block_scope_puller(root_node, ast_context, target, verbose):
    if target.has_block_scope:
        block_scope_puller = visitors.BlockScopePuller(ast_context, target)
        v.visit(root_node, _add_scope_decorator(block_scope_puller, ast_context, target))


def _run_type_visitor(root_node, ast_context, target, verbose=False):
    ast_context.clear_all()
    type_visitor = typevisitor.TypeVisitor(ast_context, target)
    v.visit(root_node, _add_scope_decorator(type_visitor, ast_context, target), verbose)


def _emit(root_node, ast_context, target):
    token_visitor = tokenvisitor.TokenVisitor(ast_context, target)
    v.visit(root_node, _add_scope_decorator(token_visitor, ast_context, target))
    tokens = token_visitor.tokens
    token_consumer = asttoken.TokenConsumer(target)
    for i, token in enumerate(tokens):
        remaining_tokens = [] if i+1 == len(tokens) else tokens[i+1:]
        token_consumer.feed(token, remaining_tokens)
    return str(token_consumer)


def _add_scope_decorator(delegate, ast_context, target):
    return visitor.scopedecorator.ScopeDecorator(delegate, ast_context, target)


def _init():
    _setup_attr_access()
    resolver = _get_builtins()
    return context.ASTContext(base_resolver=resolver)


def _get_builtins():
    resolver = resolverm.AttributeResolver()
    none_ti = ti.TypeInfo.none()
    bool_ti = ti.TypeInfo.bool()
    int_ti = ti.TypeInfo.int()
    str_ti = ti.TypeInfo.str()

    # top-level, global functions
    nomod = resolverm.NO_MODULE    
    resolver.register(nomod, function.Function.ro("enumerate",
        ti.TypeInfo.list().of(
            ti.TypeInfo.tuple().of(
                ti.TypeInfo.int(),
                ti.TypeInfo.late_resolver(lambda ati: ati.get_contained_type_info_at(0))))))
    resolver.register(nomod, function.Function.ro("input", str_ti))
    resolver.register(nomod, function.Function.ro("len", int_ti))
    resolver.register(nomod, function.Function.ro("open", ti.TypeInfo.textiowraper()))
    resolver.register(nomod, function.Function.ro("print", none_ti))
    resolver.register(nomod, function.Function.ro("range", ti.TypeInfo.list().of(int_ti)))
    resolver.register(nomod, function.Function.ro("sorted", ti.TypeInfo.late_resolver(lambda ati: ati)))
    resolver.register(nomod, function.Function.ro("str", str_ti))

    # os functions
    os_module_ti = ti.TypeInfo.module("os")
    resolver.register(os_module_ti, "sep", str_ti)
    
    # os.path functions
    os_path_module_ti = ti.TypeInfo.module("os.path")
    resolver.register(os_path_module_ti, function.Function.ro("join", str_ti, imports="os"))
    resolver.register(os_path_module_ti, "sep", str_ti)

    # str methods
    resolver.register(str_ti, function.Function.ro("endswith", bool_ti))
    resolver.register(str_ti, function.Function.ro("find", int_ti))
    resolver.register(str_ti, function.Function.ro("index", int_ti))
    resolver.register(str_ti, function.Function.ro("join", str_ti))
    resolver.register(str_ti, function.Function.ro("lower", str_ti))
    resolver.register(str_ti, function.Function.ro("split", ti.TypeInfo.list().of(str_ti)))
    resolver.register(str_ti, function.Function.ro("strip", str_ti))
    resolver.register(str_ti, function.Function.ro("startswith", bool_ti))
    resolver.register(str_ti, function.Function.ro("upper", str_ti))

    # file methods
    file_ti = ti.TypeInfo.textiowraper()
    resolver.register(file_ti, function.Function.ro("read", str_ti))
    resolver.register(file_ti, function.Function.ro("readlines", ti.TypeInfo.list().of(str_ti)))
    resolver.register(file_ti, function.Function.ro("write", none_ti))

    # list methods
    list_ti = ti.TypeInfo.list()
    resolver.register(list_ti, function.Function.ro("append", none_ti))
    resolver.register(list_ti, function.Function.ro("sort", none_ti))

    return resolver


def _setup_attr_access():

    def _get_attr(self, name):
        """
        impl notes:
          getattr and hasattr end up calling __getattribute__, therefore we
          cannot use getattr(self, ...) because it will cause infinite
          recursion; that's why we call object.__getattribute__
          getattr(v, ...), so not on self, is fine

          fun fact: __getattribute__ is called for all attribute acccess whereas
                    __getattr__ is only called (as a fallback) if the instance
                    does not have the attribute

          based on how long tests take to run, this doubles the parser's
          runtime. however, before this logic was added, the codebase was
          littered with node.get() calls to access the alternative node (if
          there is one associated with the node)
          this made the code more error prone (easy to forget the get() call)
          and harder to read
          another totally different way to deal with all of this is to rebuild
          the ast once nodes have been re-written and, that way, to avoid
          the "alternative node" chaining alltogether
        """
        val = object.__getattribute__(self, name)
        if hasattr(val, nodeattrs.ALT_NODE_ATTR):
            # since getattr calls __getattribute__, this recurses through
            # the node chain to the last alternative node
            val = getattr(val, nodeattrs.ALT_NODE_ATTR)
        return val

    astm.AST.__getattribute__ = _get_attr


    # keep the old get method around for some edge cases
    # this method returns the actual node to use, honoring the associated
    # "alternate" node, if set
    def _get_alt_node(self):
        alt = getattr(self, nodeattrs.ALT_NODE_ATTR, None)
        return self if alt is None else alt
    astm.AST.get = _get_alt_node
