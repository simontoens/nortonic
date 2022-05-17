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


def run(code, syntax, formatter=None):
    if formatter is None:
        if isinstance(syntax, syntaxm.PythonSyntax):
            formatter = syntaxm.PythonFormatter()
        elif isinstance(syntax, syntaxm.JavaSyntax):
            formatter = syntaxm.JavaFormatter()
        elif isinstance(syntax, syntaxm.ElispSyntax):
            formatter = syntaxm.ElispFormatter()
        else:
            assert False, "Unkown syntax %s" % syntax
    ast = astm.parse(code)
    ast_context = context.ASTContext()
    if syntax.has_block_scope:
        visitorm.visit(ast, visitor_decorators.ScopeDecorator(
            visitors.BlockScopePuller(ast_context, syntax), ast_context))
    visitorm.visit(ast, visitors.TypeVisitor(ast_context, syntax))
    visitorm.visit(ast, visitors.ContainerTypeVisitor(ast_context))
    visitorm.visit(ast, visitors.FuncCallVisitor(ast_context, syntax))

    token_visitor = tokenvisitors.TokenVisitor(ast_context, syntax)
    scope_decorator = visitor_decorators.ScopeDecorator(token_visitor, ast_context)    
    visitorm.visit(ast, scope_decorator)
    tokens = token_visitor.tokens
    token_consumer = asttoken.TokenConsumer(syntax, formatter)
    for i, token in enumerate(tokens):
        remaining_tokens = [] if i+1 == len(tokens) else tokens[i+1:]
        token_consumer.feed(token, remaining_tokens)
    return str(token_consumer)


def _parse_arguments(args):
    parser = argparse.ArgumentParser(description="Go, Python!")
    parser.add_argument("--python", required=False, action="store_true",
                        help="Compile to Python")
    parser.add_argument("--java", required=False, action="store_true",
                        help="Compile to Java")
    parser.add_argument("--elisp", required=False, action="store_true",
                        help="Compile to elisp")
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
        print(run(f.read(), syntax))
