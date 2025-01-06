import lang.target.templates as templates
import unittest


class ClassDeclarationTemplateTest(unittest.TestCase):

    def test_class_declaration(self):
        template = templates.ClassDeclarationTemplate("type $class_name struct")

        decl = template.render("Foo")

        self.assertEqual("type Foo struct", decl)


if __name__ == '__main__':
    unittest.main()

