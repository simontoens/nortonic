import ast as astm
import context
import emittervisitor
import syntax
import visitor as visitorm
import visitors


def run(code, language_syntax):
    ast_context = context.ASTContext()
    ast = astm.parse(code)
    visitorm.visit(ast, visitors.TypeVisitor(ast_context))
    visitor = emittervisitor.LanguageEmitterVisitor(ast_context,language_syntax)
    visitorm.visit(ast, visitor)
    return str(visitor)


if __name__ == "__main__":
    #syntax = syntax.PythonSyntax()
    syntax = syntax.JavaSyntax()
    with open("test.py", "r") as f:
        print(run(f.read(), syntax))
