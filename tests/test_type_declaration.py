import ast
import lang.scope as scopem
import lang.target.templates as templates
import unittest


class TypeDeclarationTemplateTest(unittest.TestCase):

    def setUp(self):
        self.scope = scopem.Scope(parent_scope=None, ast_node=ast.Name(), namespace=None)

    def test_type_declaration(self):
        template = templates.TypeDeclarationTemplate("var $identifier $type =")

        decl, _ = template.render("string", "s", self.scope, {})

        self.assertEqual("var s string =", decl)

    def test_with_rhs(self):
        template = templates.TypeDeclarationTemplate("blah $type = $rhs foo")

        _, process_rhs = template.render("string", "s", self.scope, {})

        self.assertTrue(process_rhs)

    def test_without_rhs(self):
        template = templates.TypeDeclarationTemplate("goo $identifier")

        _, process_rhs = template.render("string", "s", self.scope, {})

        self.assertFalse(process_rhs)


if __name__ == '__main__':
    unittest.main()

