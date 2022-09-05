import argparse
import ast as astm
import context
import tokenvisitors
import syntax as syntaxm
import sys
import asttoken
import visitor as visitorm
import visitor_decorators
import visitors


def run(code, syntax, verbose=False):
    ast_context = context.ASTContext()
    root_node = astm.parse(code)
    _pre_process(root_node, ast_context, syntax, verbose)
    return _emit(root_node, ast_context, syntax)


def _pre_process(root_node, ast_context, syntax, verbose=False):
    if syntax.has_block_scope:
        block_scope_puller = visitors.BlockScopePuller(ast_context, syntax)
        visitorm.visit(root_node, _add_scope_decorator(block_scope_puller, ast_context))
    type_visitor = visitors.TypeVisitor(ast_context, syntax)
    visitorm.visit(root_node, _add_scope_decorator(type_visitor, ast_context), verbose)
    visitorm.visit(root_node, visitors.FuncCallVisitor(ast_context, syntax), verbose)


def _emit(root_node, ast_context, syntax):
    # this is dumb, associate with syntax
    if isinstance(syntax, syntaxm.PythonSyntax):
        formatter = syntaxm.PythonFormatter()
    elif isinstance(syntax, syntaxm.JavaSyntax):
        formatter = syntaxm.JavaFormatter()
    elif isinstance(syntax, syntaxm.ElispSyntax):
        formatter = syntaxm.ElispFormatter()
    else:
        assert False, "Unkown syntax %s" % syntax
    token_visitor = tokenvisitors.TokenVisitor(ast_context, syntax)
    visitorm.visit(root_node, _add_scope_decorator(token_visitor, ast_context))
    tokens = token_visitor.tokens
    token_consumer = asttoken.TokenConsumer(syntax, formatter)
    for i, token in enumerate(tokens):
        remaining_tokens = [] if i+1 == len(tokens) else tokens[i+1:]
        token_consumer.feed(token, remaining_tokens)
    return str(token_consumer)


def _add_scope_decorator(delegate, ast_context):
    return visitor_decorators.ScopeDecorator(delegate, ast_context)


def _parse_arguments(args):
    parser = argparse.ArgumentParser(description="Go, Python!")
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
        syntax = syntaxm.PythonSyntax()
    elif args.java:
        syntax = syntaxm.JavaSyntax()
    elif args.elisp:
        syntax = syntaxm.ElispSyntax()
    else:
        raise Exception("no target specified")
    with open("test.py", "r") as f:
        print(run(f.read(), syntax, args.verbose))
