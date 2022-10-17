from tests import compilertest
import unittest


class BuiltInFuncTest(compilertest.CompilerTest):

    def test_equals_int(self):
        py = 'print(1 == 1)'
        self.py(py, py)
        self.java(py, 'System.out.println(1 == 1);')
        self.elisp(py, '(message "%s" (equal 1 1))')

    def test_equals_int_assignment(self):
        py = 'b = 1 == 1'
        self.py(py, py)
        self.java(py, 'Boolean b = 1 == 1;')
        self.elisp(py, '(setq b (equal 1 1))')

    def test_equals_string(self):
        py = 'print("nice" == "dream")'
        self.py(py, py)
        self.java(py, 'System.out.println("nice".equals("dream"));')
        self.elisp(py, '(message "%s" (equal "nice" "dream"))')

    def test_equals_string_assignment(self):
        py = 'b = "nice" == "dream"'
        self.py(py, py)
        self.java(py, 'Boolean b = "nice".equals("dream");')
        self.elisp(py, '(setq b (equal "nice" "dream"))')

    def test_len__string(self):
        py = 'print(len("four"))'
        self.py(py, py)
        self.java(py, 'System.out.println("four".length());')
        self.elisp(py, '(message "%s" (length "four"))')

    def test_len__list(self):
        py = 'print(len([1, 2, 3]))'
        self.py(py, py)
        self.java(py, 'System.out.println(new ArrayList<>(List.of(1, 2, 3)).size());')
        self.elisp(py, '(message "%s" (length (list 1 2 3)))')

    def test_len_assignment(self):
        py = 'l = len("four")'
        self.py(py, py)
        self.java(py, 'Integer l = "four".length();')
        self.elisp(py, '(setq l (length "four"))')

    def test_startswith(self):
        py = 'print("four".startswith("f"))'
        self.py(py, py)
        self.java(py, 'System.out.println("four".startsWith("f"));')
        self.elisp(py, '(message "%s" (string-prefix-p "f" "four"))')

    def test_startswith_assignment(self):
        py = 'b = "four".startswith("f")'
        self.py(py, py)
        self.java(py, 'Boolean b = "four".startsWith("f");')
        self.elisp(py, '(setq b (string-prefix-p "f" "four"))')

    def test_endswith(self):
        py = 'print("four".endswith("f"))'
        self.py(py, py)
        self.java(py, 'System.out.println("four".endsWith("f"));')
        self.elisp(py, '(message "%s" (string-suffix-p "f" "four"))')

    def test_join(self):
        py = 'print(" ".join(["batteries", "included"]))'
        self.py(py, py)
        self.java(py, 'System.out.println(String.join(" ", new ArrayList<>(List.of("batteries", "included"))));')
        self.elisp(py, '(message (mapconcat \'identity (list "batteries" "included") " "))')

    def test_split(self):
        py = 'l = "batteries included".split(" ")'
        self.py(py, py)
        self.java(py, 'List<String> l = Arrays.asList("batteries included".split(" "));')
        self.elisp(py, '(setq l (split-string "batteries included" " "))')

    def test_split_noargs(self):
        py = 'l = "batteries included".split()'
        self.py(py, py)
        self.java(py, 'List<String> l = Arrays.asList("batteries included".split(" "));')
        self.elisp(py, '(setq l (split-string "batteries included"))')

    def test_index(self):
        py = 'i = "batteries included".index("b")'
        self.py(py, py)
        self.java(py, 'Integer i = "batteries included".indexOf("b");')
        self.elisp(py, '(setq i (cl-search "b" "batteries included"))')

    def test_find(self):
        py = 'i = "batteries included".find("b")'
        self.py(py, py)
        self.java(py, 'Integer i = "batteries included".indexOf("b");')
        self.elisp(py, '(setq i (cl-search "b" "batteries included"))')

    def test_endswith_assigment(self):
        py = 'b = "four".endswith("f")'
        self.py(py, py)
        self.java(py, 'Boolean b = "four".endsWith("f");')
        self.elisp(py, '(setq b (string-suffix-p "f" "four"))')

    def test_print__single_arg_int(self):
        py = "print(1)"
        self.py(py, py)
        self.java(py, "System.out.println(1);")
        self.elisp(py, "(message \"%s\" 1)")
        self.golang(py, "fmt.Println(1)")

    def test_print__single_arg_str(self):
        py = 'print("hello")'
        self.py(py, py)
        self.java(py, "System.out.println(\"hello\");")
        self.elisp(py, "(message \"hello\")")
        self.golang(py, "fmt.Println(\"hello\")")

    def test_print__multiple_args(self):
        py = "print(1, \"foo\", 1.2)"
        self.py(py, py)
        self.java(py, "System.out.println(String.format(\"%d %s %d\", 1, \"foo\", 1.2));")
        self.elisp(py, "(message \"%s %s %s\" 1 \"foo\" 1.2)")
        self.golang(py, "fmt.Println(1, \"foo\", 1.2)")

    def test_sort_list(self):
        py = """
l = [3, 2, 1]
l.sort()
"""
        self.py(py, py)
        self.java(py, """
List<Integer> l = new ArrayList<>(List.of(3, 2, 1));
l.sort(null);
""")
        self.elisp(py, """
(setq l (list 3 2 1))
(setq l (sort l '<))
""")

    def test_chained_method_calls(self):
        py = 'b = " FOO ".lower().strip().startswith("f")'
        self.py(py, py)
        self.java(py, 'Boolean b = " FOO ".toLowerCase().trim().startsWith("f");')
        self.elisp(py, '(setq b (string-prefix-p "f" (string-trim (downcase " FOO "))))')

    def test_os_sep(self):
        py = """
import os
s = os.sep
"""
        self.py(py, "s = os.sep")
        self.java(py, 'String s = System.getProperty("file.separator");')
        self.elisp(py, '(setq s "/")')

    def test_path_sep(self):
        py = """
import os
s = os.path.sep
"""
        self.py(py, "s = os.path.sep")
        self.java(py, 'String s = File.separator;')
        self.elisp(py, '(setq s "/")')

    def test_os_path_join(self):
        py = """
import os
s = os.path.join("foo", "blah", "goo")
"""
        self.py(py, 's = os.path.join("foo", "blah", "goo")')
        self.java(py, 'String s = Paths.get("foo", "blah", "goo").toString();')
        self.elisp(py, '(setq s (f-join "foo" "blah" "goo"))')


if __name__ == '__main__':
    unittest.main()
