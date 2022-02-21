import function
import unittest


class FunctionSignatureTemplateTest(unittest.TestCase):

    def test_parse_template1(self):
        template = function.FunctionSignatureTemplate(
            "def $func_name($args_start$arg_name, $args_end)")

        self.assertEqual("def $func_name(", template.signature_beginning)
        self.assertEqual(")", template.signature_end)
        self.assertEqual("$arg_name", template.arg_template)
        self.assertEqual(", ", template.arg_sep)

    def test_parse_template2(self):
        template = function.FunctionSignatureTemplate(
            "(defun $func_name $args_start$arg_name $args_end)")

        self.assertEqual("(defun $func_name ", template.signature_beginning)
        self.assertEqual(")", template.signature_end)
        self.assertEqual("$arg_name", template.arg_template)
        self.assertEqual(" ", template.arg_sep)

    def test_parse_template3(self):
        template = function.FunctionSignatureTemplate(
            "$func_name $args_start$arg_type $arg_name+$args_endfoo")

        self.assertEqual("$func_name ", template.signature_beginning)
        self.assertEqual("foo", template.signature_end)
        self.assertEqual("$arg_type $arg_name", template.arg_template)
        self.assertEqual("+", template.arg_sep)

    def test_render_template_python(self):
        template = function.FunctionSignatureTemplate(
            "def $func_name($args_start$arg_name, $args_end)")

        signature = template.render("myfunc", (("a1", "str"), ("a2", "int"),))

        self.assertEqual("def myfunc(a1, a2)", signature)

    def test_render_template_java(self):
        template = function.FunctionSignatureTemplate(
            "$visibility $rtn_type $func_name($args_start$arg_type $arg_name, $args_end)")

        signature = template.render("myfunc", (("a1", "String"), ("a2", "int"),))

        self.assertEqual("public void myfunc(String a1, int a2)", signature)

    def test_render_template_elisp(self):
        template = function.FunctionSignatureTemplate(
            "(defun $func_name $args_start$arg_name $args_end)")

        signature = template.render("myfunc", (("a1", "String"), ("a2", "int"),))

        self.assertEqual("(defun myfunc a1 a2)", signature)


if __name__ == '__main__':
    unittest.main()

