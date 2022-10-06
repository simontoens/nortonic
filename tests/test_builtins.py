from run import run
import syntax as syntaxm
import unittest


class BuiltInFuncTest(unittest.TestCase):

    def test_equals_int(self):
        py = 'print(1 == 1)'
        self._t(py, py, syntaxm.PythonSyntax())
        self._t(py, 'System.out.println(1 == 1);', syntaxm.JavaSyntax())
        self._t(py, '(message "%s" (equal 1 1))', syntaxm.ElispSyntax())

    def test_equals_int_assignment(self):
        py = 'b = 1 == 1'
        self._t(py, py, syntaxm.PythonSyntax())
        self._t(py, 'Boolean b = 1 == 1;', syntaxm.JavaSyntax())
        self._t(py, '(setq b (equal 1 1))', syntaxm.ElispSyntax())

    def test_equals_string(self):
        py = 'print("nice" == "dream")'
        self._t(py, py, syntaxm.PythonSyntax())
        self._t(py, 'System.out.println("nice".equals("dream"));', syntaxm.JavaSyntax())
        self._t(py, '(message "%s" (equal "nice" "dream"))', syntaxm.ElispSyntax())

    def test_equals_string_assignment(self):
        py = 'b = "nice" == "dream"'
        self._t(py, py, syntaxm.PythonSyntax())
        self._t(py, 'Boolean b = "nice".equals("dream");', syntaxm.JavaSyntax())
        self._t(py, '(setq b (equal "nice" "dream"))', syntaxm.ElispSyntax())

    def test_len__string(self):
        py = 'print(len("four"))'
        self._t(py, py, syntaxm.PythonSyntax())
        self._t(py, 'System.out.println("four".length());', syntaxm.JavaSyntax())
        self._t(py, '(message "%s" (length "four"))', syntaxm.ElispSyntax())

    def test_len__list(self):
        py = 'print(len([1, 2, 3]))'
        self._t(py, py, syntaxm.PythonSyntax())
        self._t(py, 'System.out.println(new ArrayList<>(List.of(1, 2, 3)).size());', syntaxm.JavaSyntax())
        self._t(py, '(message "%s" (length (list 1 2 3)))', syntaxm.ElispSyntax())

    def test_len_assignment(self):
        py = 'l = len("four")'
        self._t(py, py, syntaxm.PythonSyntax())
        self._t(py, 'Integer l = "four".length();', syntaxm.JavaSyntax())
        self._t(py, '(setq l (length "four"))', syntaxm.ElispSyntax())

    def test_startswith(self):
        py = 'print("four".startswith("f"))'
        self._t(py, py, syntaxm.PythonSyntax())
        self._t(py, 'System.out.println("four".startsWith("f"));', syntaxm.JavaSyntax())
        self._t(py, '(message "%s" (string-prefix-p "f" "four"))', syntaxm.ElispSyntax())

    def test_startswith_assignment(self):
        py = 'b = "four".startswith("f")'
        self._t(py, py, syntaxm.PythonSyntax())
        self._t(py, 'Boolean b = "four".startsWith("f");', syntaxm.JavaSyntax())
        self._t(py, '(setq b (string-prefix-p "f" "four"))', syntaxm.ElispSyntax())

    def test_endswith(self):
        py = 'print("four".endswith("f"))'
        self._t(py, py, syntaxm.PythonSyntax())
        self._t(py, 'System.out.println("four".endsWith("f"));', syntaxm.JavaSyntax())
        self._t(py, '(message "%s" (string-suffix-p "f" "four"))', syntaxm.ElispSyntax())

    def test_join(self):
        py = 'print(" ".join(["batteries", "included"]))'
        self._t(py, py, syntaxm.PythonSyntax())
        self._t(py, 'System.out.println(String.join(" ", new ArrayList<>(List.of("batteries", "included"))));', syntaxm.JavaSyntax())
        self._t(py, '(message (mapconcat \'identity (list "batteries" "included") " "))', syntaxm.ElispSyntax())

    def test_split(self):
        py = 'l = "batteries included".split(" ")'
        self._t(py, py, syntaxm.PythonSyntax())
        self._t(py, 'List<String> l = Arrays.asList("batteries included".split(" "));', syntaxm.JavaSyntax())
        self._t(py, '(setq l (split-string "batteries included" " "))', syntaxm.ElispSyntax())

    def test_split_noargs(self):
        py = 'l = "batteries included".split()'
        self._t(py, py, syntaxm.PythonSyntax())
        self._t(py, 'List<String> l = Arrays.asList("batteries included".split(" "));', syntaxm.JavaSyntax())
        self._t(py, '(setq l (split-string "batteries included"))', syntaxm.ElispSyntax())

    def test_index(self):
        py = 'i = "batteries included".index("b")'
        self._t(py, py, syntaxm.PythonSyntax())
        self._t(py, 'Integer i = "batteries included".indexOf("b");', syntaxm.JavaSyntax())
        self._t(py, '(setq i (cl-search "b" "batteries included"))', syntaxm.ElispSyntax())

    def test_find(self):
        py = 'i = "batteries included".find("b")'
        self._t(py, py, syntaxm.PythonSyntax())
        self._t(py, 'Integer i = "batteries included".indexOf("b");', syntaxm.JavaSyntax())
        self._t(py, '(setq i (cl-search "b" "batteries included"))', syntaxm.ElispSyntax())

    def test_endswith_assigment(self):
        py = 'b = "four".endswith("f")'
        self._t(py, py, syntaxm.PythonSyntax())
        self._t(py, 'Boolean b = "four".endsWith("f");', syntaxm.JavaSyntax())
        self._t(py, '(setq b (string-suffix-p "f" "four"))', syntaxm.ElispSyntax())

    def test_print__single_arg_int(self):
        py = "print(1)"
        self._t(py, py, syntaxm.PythonSyntax())
        self._t(py, "System.out.println(1);", syntaxm.JavaSyntax())
        self._t(py, "(message \"%s\" 1)", syntaxm.ElispSyntax())

    def test_print__single_arg_str(self):
        py = 'print("hello")'
        self._t(py, py, syntaxm.PythonSyntax())
        self._t(py, "System.out.println(\"hello\");", syntaxm.JavaSyntax())
        self._t(py, "(message \"hello\")", syntaxm.ElispSyntax())

    def test_print__multiple_args(self):
        py = "print(1, \"foo\", 1.2)"
        self._t(py, py, syntaxm.PythonSyntax())
        self._t(py, "System.out.println(String.format(\"%d %s %d\", 1, \"foo\", 1.2));", syntaxm.JavaSyntax())
        self._t(py, "(message \"%s %s %s\" 1 \"foo\" 1.2)", syntaxm.ElispSyntax())

    def test_sort_list(self):
        py = """
l = [3, 2, 1]
l.sort()
"""
        self._t(py, py, syntaxm.PythonSyntax())
        self._t(py, """
List<Integer> l = new ArrayList<>(List.of(3, 2, 1));
l.sort(null);
""", syntaxm.JavaSyntax())
        self._t(py, """
(setq l (list 3 2 1))
(setq l (sort l '<))
""", syntaxm.ElispSyntax())

    def test_chained_method_calls(self):
        py = 'b = " FOO ".lower().strip().startswith("f")'
        self._t(py, py, syntax=syntaxm.PythonSyntax())
        self._t(py, 'Boolean b = " FOO ".toLowerCase().trim().startsWith("f");', syntaxm.JavaSyntax())
        self._t(py, '(setq b (string-prefix-p "f" (string-trim (downcase " FOO "))))', syntaxm.ElispSyntax())

    def test_os_sep(self):
        py = """
import os
s = os.sep
"""
        self._t(py, "s = os.sep", syntax=syntaxm.PythonSyntax())
        self._t(py, 'String s = System.getProperty("file.separator");', syntaxm.JavaSyntax())
        self._t(py, '(setq s "/")', syntaxm.ElispSyntax())

    def test_path_sep(self):
        py = """
import os
s = os.path.sep
"""
        self._t(py, "s = os.path.sep", syntax=syntaxm.PythonSyntax())
        self._t(py, 'String s = File.separator;', syntaxm.JavaSyntax())
        self._t(py, '(setq s "/")', syntaxm.ElispSyntax())

    def test_os_path_join(self):
        py = """
import os
s = os.path.join("foo", "blah", "goo")
"""
        self._t(py, 's = os.path.join("foo", "blah", "goo")', syntax=syntaxm.PythonSyntax())
        self._t(py, 'String s = Paths.get("foo", "blah", "goo").toString();', syntaxm.JavaSyntax())
        self._t(py, '(setq s (f-join "foo" "blah" "goo"))', syntaxm.ElispSyntax())

    def _t(self, code, expected, syntax):
        generated_code = run(code, syntax)

        self.assertEqual(expected.strip(), generated_code)


if __name__ == '__main__':
    unittest.main()
