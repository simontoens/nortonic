import ast as astm
import emittervisitor
import syntax
import visitor as visitorm


def run(code, language_syntax):
    ast = astm.parse(code)
    visitor = emittervisitor.LanguageEmitterVisitor(language_syntax)
    visitorm.visit(ast, visitor)
    return str(visitor)


if __name__ == "__main__":
    #syntax = PythonSyntax()
    syntax = syntax.JavaSyntax()
    with open("test.py", "r") as f:
        print(run(f.read(), syntax))
