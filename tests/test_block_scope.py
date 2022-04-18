from run import *
import ast as astm
import syntax as syntaxm
import unittest


class BlockScopeTest(unittest.TestCase):

    def test_if_single_stmt__1(self):
        py = """
name = "smoke"
if name == "water":
    name = "water"
print(name)
"""
        self._t(py, syntax=syntaxm.PythonSyntax(), expected=py)

        self._t(py, syntax=syntaxm.JavaSyntax(), expected="""
String name = "smoke";
if (name.equals("water")) {
    name = "water";
}
System.out.println(name);
""")

        self._t(py, syntax=syntaxm.ElispSyntax(), expected="""
(setq name "smoke")
(if (equal name "water")
    (setq name "water"))
(message name)
""")

    def _t(self, code, expected, syntax):
        generated_code = run(code, syntax)

        self.assertEqual(expected.strip(), generated_code)


if __name__ == '__main__':
    unittest.main()
