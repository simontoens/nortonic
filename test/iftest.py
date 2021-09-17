from run import *
import ast as astm
import syntax as syntaxm
import unittest


class IfTest(unittest.TestCase):

    def test_if(self):
        py = """
name="smoke"
if name == "water":
    if 1==1:
        return True
    else:
        return False
"""
        self._t(py, syntax=syntaxm.PythonSyntax(), expected="""
name = "smoke"
if name == "water":
    if 1 == 1:
        return True
    else:
        return False""")

        # TODO replace == with .equals based on type
        self._t(py, syntax=syntaxm.JavaSyntax(), expected="""
String name = "smoke";
if (name == "water") {
    if (1 == 1) {
        return true;
    } else {
        return false;
    }
}
""")
        
                   
    def test_elif(self):
        """
        In the AST elif has been normalized to nested if/else.
        """
        py = """
if 1==1:
    return True
elif 1==2:
    return False
else:
    return 3
"""
        self._t(py, syntax=syntaxm.PythonSyntax(), expected="""
if 1 == 1:
    return True
else:
    if 1 == 2:
        return False
    else:
        return 3
""")

        self._t(py, syntax=syntaxm.JavaSyntax(), expected="""
if (1 == 1) {
    return true;
} else {
    if (1 == 2) {
        return false;
    } else {
        return 3;
    }
}
""")

    def _t(self, code, expected, syntax):
        generated_code = run(code, syntax)

        self.assertEqual(expected.strip(), generated_code)


if __name__ == '__main__':
    unittest.main()
