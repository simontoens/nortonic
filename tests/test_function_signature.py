import lang.target.templates as templates
import unittest


class FunctionSignatureTemplateTest(unittest.TestCase):

    def test_parse_template1(self):
        template = templates.FunctionSignatureTemplate(
            "def $func_name($args_start$arg_name, $args_end)")

        self.assertEqual("def $func_name(", template.signature_beginning)
        self.assertEqual(")", template.signature_end)
        self.assertEqual("$arg_name", template.arg_template)
        self.assertEqual(", ", template.arg_sep)

    def test_parse_template2(self):
        template = templates.FunctionSignatureTemplate(
            "(defun $func_name $args_start$arg_name $args_end)")

        self.assertEqual("(defun $func_name ", template.signature_beginning)
        self.assertEqual(")", template.signature_end)
        self.assertEqual("$arg_name", template.arg_template)
        self.assertEqual(" ", template.arg_sep)

    def test_parse_template3(self):
        template = templates.FunctionSignatureTemplate(
            "$func_name $args_start$arg_type $arg_name+$args_endfoo")

        self.assertEqual("$func_name ", template.signature_beginning)
        self.assertEqual("foo", template.signature_end)
        self.assertEqual("$arg_type $arg_name", template.arg_template)
        self.assertEqual("+", template.arg_sep)

    def test_render_template_python_without_args(self):
        template = templates.FunctionSignatureTemplate(
            "def $func_name($args_start$arg_name, $args_end)")

        signature = template.render("myfunc", (), None, "public", scope=None, node_attrs=None)

        self.assertEqual("def myfunc()", signature)

    def test_render_template_anonymous_function(self):
        template = templates.FunctionSignatureTemplate(
            "lambda $args_start$arg_name,$args_end")

        signature = template.render("myfunc", (), None, "public", scope=None, node_attrs=None)

        self.assertEqual("lambda", signature)

    def test_render_template_anonymous_function__args(self):
        template = templates.FunctionSignatureTemplate(
            "lambda $args_start$arg_name,$args_end:")

        signature = template.render("myfunc", (("a1", "str"), ("a2", "str")), None, "public", scope=None, node_attrs=None)

        self.assertEqual("lambda a1,a2:", signature)

    def test_render_template_anonymous_function__space_after_comma_sep(self):
        template = templates.FunctionSignatureTemplate(
            "lambda $args_start$arg_name, $args_end:")

        signature = template.render("myfunc", (), None, "public", scope=None, node_attrs=None)

        self.assertEqual("lambda:", signature)

    def test_render_template_python_with_args(self):
        template = templates.FunctionSignatureTemplate(
            "def $func_name($args_start$arg_name, $args_end)")

        signature = template.render("myfunc", (("a1", "str"), ("a2", "int"),), None, "public", scope=None, node_attrs=None)

        self.assertEqual("def myfunc(a1, a2)", signature)

    def test_render_template_python_with_args(self):
        template = templates.FunctionSignatureTemplate(
            "def $func_name($args_start$arg_name, $args_end)")

        signature = template.render("myfunc", (("a1", "str"), ("a2", "int"),), None, "public", scope=None, node_attrs=None)

        self.assertEqual("def myfunc(a1, a2)", signature)

    def test_render_template_java(self):
        template = templates.FunctionSignatureTemplate(
            "$visibility $rtn_type:void $func_name($args_start$arg_type $arg_name, $args_end)")

        signature = template.render("myfunc", (("a1", "String"), ("a2", "int"),), None, "public", scope=None, node_attrs=None)

        self.assertEqual("public void myfunc(String a1, int a2)", signature)
 
    def test_render_template_java__rtn_type_and_visibility(self):
        template = templates.FunctionSignatureTemplate(
            "$visibility $rtn_type $func_name($args_start$arg_type $arg_name, $args_end)")

        signature = template.render("myfunc", (("a1", "String"), ("a2", "int"),), visibility="private", rtn_type="Integer", scope=None, node_attrs=None)

        self.assertEqual("private Integer myfunc(String a1, int a2)", signature)

    def test_render_template_elisp(self):
        template = templates.FunctionSignatureTemplate(
            "(defun $func_name $args_start$arg_name $args_end)")

        signature = template.render("myfunc", (("a1", "String"), ("a2", "int"),), None, "public", scope=None, node_attrs=None)

        self.assertEqual("(defun myfunc a1 a2)", signature)


if __name__ == '__main__':
    unittest.main()
