from run import *
import ast as astm
import unittest


class IfTest(unittest.TestCase):

    def test_if(self):
        self._test(code="""
if __name__=="__main__":
    if 1==1:
        return True
    else:
        return False
""", expected="""
if __name__=="__main__":
    if 1==1:
        return True
    else:
        return False""")

    def test_elif(self):
        """
        In the AST elif has been normalized to nested if/else.
        """
        self._test(code="""
if 1==1:
    return True
elif 1==2:
    return False
else:
    return 3
""", expected="""
if 1==1:
    return True
else:
    if 1==2:
        return False
    else:
        return 3""")

    def _test(self, code, expected):
        generated_code = run(code, PythonSyntax())

        self.assertEqual(expected.strip(), generated_code)


if __name__ == '__main__':
    unittest.main()
