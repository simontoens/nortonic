from tests import compilertest
import unittest


class StrTest(compilertest.CompilerTest):

    def test_slice(self):
        py = """
s = "Kaito"
s2 = s[1:2]
"""
        self.py(py, expected=py)
        self.java(py, expected="""
String s = "Kaito";
String s2 = s.substring(1, 2);
""")
        self.elisp(py, expected="""
(setq s "Kaito")
(setq s2 (substring s 1 2))
""")

    def test_slice__no_upper_index(self):
        py = """
s = "Kaito"
s2 = s[1:]
"""
        self.py(py, expected=py)
        self.java(py, expected="""
String s = "Kaito";
String s2 = s.substring(1);
""")
        self.elisp(py, expected="""
(setq s "Kaito")
(setq s2 (substring s 1))
""")

    def test_slice__negative_2nd_index(self):
        py = """
s = "Kaito"
s2 = "arub" + s[1:-2] + "to"
"""
        self.py(py, expected=py)
        self.java(py, expected="""
String s = "Kaito";
String s2 = "arub" + s.substring(1, s.length() - 2) + "to";
""")
        self.elisp(py, expected="""
(setq s "Kaito")
(setq s2 (concat (concat "arub" (substring s 1 -2)) "to"))
""")


if __name__ == '__main__':
    unittest.main()
