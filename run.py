import argparse
import ast as astm
import sys

from target import elisp, golang, java, python
import asttoken
import context
import nodeattrs
import tokenvisitors
import visitor as visitorm
import visitor_decorators
import visitors


def run(code, syntax, verbose=False):
    # TODO move this out of here - can this live in an __init__.py?
    # some tests do not run this code path, they only pass because we're lucky!
    #
    # adds a "get" method to the base class of all ast nodes.
    # this method returns the actual node to use, honoring the associated
    # "alternate" node, if set
    astm.AST.get = lambda self: self if hasattr(self, nodeattrs.REWRITTEN_NODE_ATTR) else getattr(self, nodeattrs.ALT_NODE_ATTR, self)
    def _get_md(n):
        if not hasattr(n, nodeattrs.METADATA_NODE_ATTR):
            setattr(n, nodeattrs.METADATA_NODE_ATTR, {})
        return getattr(n, nodeattrs.METADATA_NODE_ATTR)
    astm.AST.get_metadata = _get_md
    ast_context = context.ASTContext()
    root_node = astm.parse(code)
    _pre_process(root_node, ast_context, syntax, verbose)
    _post_process(root_node, ast_context, syntax, verbose)
    return _emit(root_node, ast_context, syntax)


def _pre_process(root_node, ast_context, syntax, verbose=False):
    ident_collector = visitors.IdentifierCollector(ast_context)
    visitorm.visit(root_node, _add_scope_decorator(ident_collector, ast_context, syntax), verbose)
    # hack until we support "with" etc
    remover = visitors.WithRemover(ast_context)
    visitorm.visit(root_node, _add_scope_decorator(remover, ast_context, syntax), verbose)
    unpacking_rewriter = visitors.UnpackingRewriter(
        ast_context,
        syntax.has_assignment_lhs_unpacking,
        syntax.function_can_return_multiple_values)
    visitorm.visit(root_node, _add_scope_decorator(unpacking_rewriter, ast_context, syntax), verbose)
    if syntax.has_block_scope:
        block_scope_puller = visitors.BlockScopePuller(ast_context, syntax)
        visitorm.visit(root_node, _add_scope_decorator(block_scope_puller, ast_context, syntax))
    type_visitor = visitors.TypeVisitor(ast_context, syntax)
    visitorm.visit(root_node, _add_scope_decorator(type_visitor, ast_context, syntax), verbose)
    func_call_visitor = visitors.FuncCallVisitor(ast_context, syntax)
    visitorm.visit(root_node, _add_scope_decorator(func_call_visitor, ast_context, syntax), verbose)
    visitorm.visit(root_node, visitors.DocStringHandler(ast_context), verbose)


def _post_process(root_node, ast_context, syntax, verbose=False):
    for v in syntax.visitors:
        if hasattr(v, "context"):
            assert v.context is None
            v.context = ast_context
        visitorm.visit(root_node, _add_scope_decorator(v, ast_context, syntax), verbose)


def _emit(root_node, ast_context, syntax):
    token_visitor = tokenvisitors.TokenVisitor(ast_context, syntax)
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
