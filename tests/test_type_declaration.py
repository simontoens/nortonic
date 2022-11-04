import scopes
import templates
import unittest


class TypeDeclarationTemplate(unittest.TestCase):

    def test_function_scope(self):
        template = templates.TypeDeclarationTemplate("var $identifier $type =")

        decl = template.render("string", "s", scopes.FUNCTION, {})

        self.assertEqual('var s string =', decl)


if __name__ == '__main__':
    unittest.main()

