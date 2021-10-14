import ast as astm
import context
import tokenvisitors
import syntax as syntaxm
import ast_token
import visitor as visitorm
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
    visitorm.visit(ast, visitors.TypeVisitor(ast_context))
    visitorm.visit(ast, visitors.FuncCallVisitor(ast_context, syntax))
    token_visitor = tokenvisitors.TokenVisitor(ast_context, syntax)        
    visitorm.visit(ast, token_visitor)
    tokens = token_visitor.tokens
    token_consumer = ast_token.TokenConsumer(syntax, formatter)    
    for i, token in enumerate(tokens):
        remaining_tokens = [] if i+1 == len(tokens) else tokens[i+1:]
        token_consumer.feed(token, remaining_tokens)
    return str(token_consumer)


if __name__ == "__main__":
    #syntax = syntaxm.PythonSyntax()
    syntax = syntaxm.JavaSyntax()
    #syntax = syntaxm.ElispSyntax()
    with open("test.py", "r") as f:
        print(run(f.read(), syntax))
