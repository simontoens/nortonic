import ast as astm
import context
import emittervisitor
import syntax
import ast_token
import visitor as visitorm
import visitors


def run(code, language_syntax):
    ast_context = context.ASTContext()
    ast = astm.parse(code)
    visitorm.visit(ast, visitors.TypeVisitor(ast_context))
    formatter = ast_token.Formatter(language_syntax)
    visitor = emittervisitor.LanguageEmitterVisitor(ast_context,
                                                    language_syntax,
                                                    formatter)
    visitorm.visit(ast, visitor)
    return str(formatter)
    #return str(visitor)


if __name__ == "__main__":
    #syntax = syntax.PythonSyntax()
    syntax = syntax.JavaSyntax()
    #syntax = syntax.ElispSyntax()    
    with open("test.py", "r") as f:
        print(run(f.read(), syntax))
