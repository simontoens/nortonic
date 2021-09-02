import ast as astm
import context
import emittervisitor
import syntax as syntaxm
import ast_token
import visitor as visitorm
import visitors


def run(code, syntax, formatter):
    ast_context = context.ASTContext()
    ast = astm.parse(code)
    visitorm.visit(ast, visitors.TypeVisitor(ast_context))
    token_consumer = ast_token.TokenConsumer(syntax, formatter)
    visitor = emittervisitor.LanguageEmitterVisitor(ast_context,
                                                    syntax,
                                                    token_consumer)
    visitorm.visit(ast, visitor)
    visitor.done()
    return str(token_consumer)


if __name__ == "__main__":
    #syntax = syntaxm.PythonSyntax()
    #formatter = syntaxm.PythonFormatter()
    syntax = syntaxm.JavaSyntax()
    formatter = syntaxm.JavaFormatter()    
    #syntax = syntax.ElispSyntax()    
    with open("test.py", "r") as f:
        print(run(f.read(), syntax, formatter))
