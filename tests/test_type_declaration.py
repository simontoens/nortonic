import templates
import unittest


class TypeDeclarationTemplate(unittest.TestCase):

    def test_render(self):
        t = templates.TypeDeclarationTemplate("var $identifier $type =",
                                              "$identifier :=")

        explicit = t.render_with_type_declaration("string", "s")
        inferred = t.render_with_type_inference("s")

        self.assertEqual('var s string =', explicit)
        self.assertEqual('s :=', inferred)


if __name__ == '__main__':
    unittest.main()

