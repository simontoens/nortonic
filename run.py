import ast as astm
import context
import tokenvisitors
import syntax as syntaxm
import ast_token
import visitor as visitorm
import visitors


def run(code, syntax, formatter):
    ast_context = context.ASTContext()
    ast = astm.parse(code)
    visitorm.visit(ast, visitors.TypeVisitor(ast_context))
    token_consumer = ast_token.TokenConsumer(syntax, formatter)
    if syntax.is_prefix:
        pass
    else:
        visitor = tokenvisitors.InfixVisitor(ast_context, syntax)
    visitorm.visit(ast, visitor)
    tokens = visitor.tokens
    for i, token in enumerate(tokens):
        next_token = None if i+1 == len(tokens) else tokens[i+1]
        token_consumer.feed(token, next_token)
    return str(token_consumer)


# def greet(name: str) -> str:
#     return "hello " + name

if __name__ == "__main__":
    #syntax = syntaxm.PythonSyntax()
    #formatter = syntaxm.PythonFormatter()
    syntax = syntaxm.JavaSyntax()
    formatter = syntaxm.JavaFormatter()    
    #syntax = syntax.ElispSyntax()    
    with open("test.py", "r") as f:
        print(run(f.read(), syntax, formatter))
