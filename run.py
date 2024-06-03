import argparse
import ast as astm
import sys

from target import elisp, golang, java, python
from visitor import tokenvisitor
from visitor import typevisitor
from visitor import visitor as visitorm
from visitor import visitor_decorators
from visitor import visitors
import asttoken
import context
import nodeattrs


def run(code, syntax, verbose=False):
    ast_context = context.ASTContext()
    root_node = astm.parse(code)
    _setup()
    _check_for_obvious_errors(root_node, ast_context, verbose)
    _pre_process(root_node, ast_context, syntax, verbose)
    _post_process(root_node, ast_context, syntax, verbose)
    return _emit(root_node, ast_context, syntax)


def _setup():
    # TODO move this out of here - can this live in an __init__.py?
    # some tests do not run this code path, they only pass because we're lucky?

    def _get_attr(self, name):
        """
        impl notes:
          getattr and hasattr ends up calling __getattribute__, therefore we
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


def _check_for_obvious_errors(root_node, ast_context, verbose=False):
    pys = python.PythonSyntax()
    lame_checker = visitors.LameSemanticCheckerVisitor(ast_context, pys)
    visitorm.visit(root_node, _add_scope_decorator(lame_checker, ast_context, pys), verbose)


def _pre_process(root_node, ast_context, syntax, verbose=False):
    ident_collector = visitors.IdentifierCollector(ast_context)
    visitorm.visit(root_node, _add_scope_decorator(ident_collector, ast_context, syntax), verbose)
    # hack until we support "with" etc
    remover = visitors.WithRemover(ast_context)
    visitorm.visit(root_node, _add_scope_decorator(remover, ast_context, syntax), verbose)

    _run_block_scope_puller(root_node, ast_context, syntax, verbose)

    # needs to run before type visitor runs for the first time
    container_type_visitor = visitors.ContainerTypeVisitor(ast_context)
    visitorm.visit(root_node, container_type_visitor, verbose)
    ast_context.clear_all()
    # can we run type visitor once after block scope and unpacking?    
    _run_type_visitor(root_node, ast_context, syntax, verbose)

    ast_context.clear_all()
    _run_type_visitor(root_node, ast_context, syntax, verbose)

    # requires: type visitor
    # required by: unpacking rewriter visitor
    visitorm.visit(root_node, visitors.CallsiteVisitor(), verbose)

    unpacking_rewriter = visitors.UnpackingRewriter(ast_context, syntax)
    visitorm.visit(root_node, _add_scope_decorator(unpacking_rewriter, ast_context, syntax), verbose)

    # unpacking creates new ast nodes, they need to get associated types
    ast_context.clear_all()    
    _run_type_visitor(root_node, ast_context, syntax, verbose)

    if not syntax.has_if_expr and not syntax.ternary_replaces_if_expr:
        # review why this rewrite rule cannot move up
        visitorm.visit(root_node, visitors.IfExprRewriter(ast_context), verbose)
        _run_block_scope_puller(root_node, ast_context, syntax, verbose)
        ast_context.clear_all()    
        _run_type_visitor(root_node, ast_context, syntax, verbose)

    func_call_visitor = visitors.FuncCallVisitor(ast_context, syntax)
    visitorm.visit(root_node, _add_scope_decorator(func_call_visitor, ast_context, syntax), verbose)

    rtn_values_updater = visitors.ReturnValueMapper(ast_context)
    visitorm.visit(root_node, rtn_values_updater, verbose)
    ast_context.clear_all()    
    _run_type_visitor(root_node, ast_context, syntax, verbose)

    # needs to re-run after ReturnValueMapper because ReturnValueMapper adds
    # new node that may have to be translated (for ex assignment -> call for
    # elisp) - ReturnValueMapper cannot run before the first FuncCallVisitor
    # runs: FuncCallVisitor runs the rewrites that add inputs for
    # ReturnValueMapper (old and new values)
    func_call_visitor = visitors.FuncCallVisitor(ast_context, syntax)
    visitorm.visit(root_node, _add_scope_decorator(func_call_visitor, ast_context, syntax), verbose)

    if syntax.has_pointers:
        # this has to run after FuncCallVisitor because FuncCallVisitor may
        # add new assignments
        pointer_visitor = visitors.PointerVisitor(ast_context)
        visitorm.visit(root_node, pointer_visitor, verbose)

        # we have to relax type checking a bit here because this typevisitor
        # pass will potentially change function signature arguments to pointers
        # but callsite types are left alone
        # see test_go.py
        context.TypeInfo.TYPE_EQUALITY_CHECK_INCLUDES_POINTERS = False
        ast_context.clear_all()
        _run_type_visitor(root_node, ast_context, syntax, verbose)

        # this visitor marks all nodes that require "address of" or pointer
        # dereferencing, based on the fact that function argument types
        # are now pointers
        pointer_handler_visitor = visitors.PointerHandlerVisitor(ast_context)
        visitorm.visit(root_node, _add_scope_decorator(pointer_handler_visitor, ast_context, syntax), verbose)
        # now that the visitor above added metadata for function callsites
        # we can re-enagle stricter type checking
        context.TypeInfo.TYPE_EQUALITY_CHECK_INCLUDES_POINTERS = True

    ast_context.clear_all()
    _run_type_visitor(root_node, ast_context, syntax, verbose)

    # requires: type visitor
    # required by: token visitor
    visitorm.visit(root_node, visitors.CallsiteVisitor(), verbose)

    visitorm.visit(root_node, visitors.LambdaReturnVisitor(ast_context, syntax), verbose)
    visitorm.visit(root_node, visitors.DocStringHandler(ast_context), verbose)


def _run_block_scope_puller(root_node, ast_context, syntax, verbose):
    if syntax.has_block_scope:
        block_scope_puller = visitors.BlockScopePuller(ast_context, syntax)
        visitorm.visit(root_node, _add_scope_decorator(block_scope_puller, ast_context, syntax))


def _run_type_visitor(root_node, ast_context, syntax, verbose=False):
    type_visitor = typevisitor.TypeVisitor(ast_context, syntax)
    visitorm.visit(root_node, _add_scope_decorator(type_visitor, ast_context, syntax), verbose)


def _post_process(root_node, ast_context, syntax, verbose=False):
    for v in syntax.visitors:
        if hasattr(v, "context"):
            # if this visitor has a context field, it wants the context!
            assert v.context is None
            v.context = ast_context
        visitorm.visit(root_node, _add_scope_decorator(v, ast_context, syntax), verbose)

    ast_context.clear_all()
    _run_type_visitor(root_node, ast_context, syntax, verbose)
    # requires: type visitor
    # required by: token visitor
    visitorm.visit(root_node, visitors.CallsiteVisitor(), verbose)


    if syntax.ternary_replaces_if_expr:
        # has to run late because it changes the ast in such a way that the
        # type visitor gets confused
        # this is unfortunate and we are trying hard to avoid ast modifications
        # that make it impossible to then further process the ast        
        visitorm.visit(root_node, visitors.IfExprToTernaryRewriter(), verbose)


def _emit(root_node, ast_context, syntax):
    token_visitor = tokenvisitor.TokenVisitor(ast_context, syntax)
    visitorm.visit(root_node, _add_scope_decorator(token_visitor, ast_context, syntax))
    tokens = token_visitor.tokens
    token_consumer = asttoken.TokenConsumer(syntax)
    for i, token in enumerate(tokens):
        remaining_tokens = [] if i+1 == len(tokens) else tokens[i+1:]
        token_consumer.feed(token, remaining_tokens)
    return str(token_consumer)


def _add_scope_decorator(delegate, ast_context, syntax):
    return visitor_decorators.ScopeDecorator(delegate, ast_context, syntax)


def _parse_arguments(args):
    parser = argparse.ArgumentParser(description="Go, Python!")
    parser.add_argument("--go", required=False, action="store_true",
                        help="compile to Golang!")    
    parser.add_argument("--python", required=False, action="store_true",
                        help="compile to Python")
    parser.add_argument("--java", required=False, action="store_true",
                        help="compile to Java")
    parser.add_argument("--elisp", required=False, action="store_true",
                        help="compile to elisp")
    parser.add_argument("--verbose", required=False, action="store_true",
                        help="verbose output")
    return parser.parse_args()


if __name__ == "__main__":
    args = _parse_arguments(sys.argv)
    if args.python:
        syntax = python.PythonSyntax()
    elif args.java:
        syntax = java.JavaSyntax()
    elif args.elisp:
        syntax = elisp.ElispSyntax()
    elif args.go:
        syntax = golang.GolangSyntax()
    else:
        raise Exception("no target specified")

    with open("test.py", "r") as f:
        print(run(f.read(), syntax, args.verbose))
