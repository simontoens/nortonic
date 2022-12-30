import ast
import scope as scopem
import templates
import unittest


class TypeDeclarationTemplate(unittest.TestCase):

    def test_type_declaration(self):
        scope = scopem.Scope(parent_scope=None, ast_node=ast.Name(), namespace=None)
        template = templates.TypeDeclarationTemplate("var $identifier $type =")

        decl = template.render("string", "s", scope, {})

        self.assertEqual('var s string =', decl)


if __name__ == '__main__':
    unittest.main()

