from run import *
import ast as astm
import syntax as syntaxm
import unittest


class IfTest(unittest.TestCase):

    def test_if(self):
        code="""
if name == "water":
    if 1==1:
        return True
    else:
        return False
"""
        self._test(code,
                   expected="""
if name=="water":
    if 1==1:
        return True
    else:
        return False""",
                   syntax=syntaxm.PythonSyntax())

#         self._test(code,
#                    expected="""
# if name=="water":
#     if 1==1{
#         return True;
#     }else{
#         return False;
#     }""",
#                    syntax=syntaxm.JavaSyntax())
        

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
""",
                   expected="""
if 1==1:
    return True
else:
    if 1==2:
        return False
    else:
        return 3
""",
                   syntax=syntaxm.PythonSyntax())

    def _test(self, code, expected, syntax):
        generated_code = run(code, syntax)

        self.assertEqual(expected.strip(), generated_code)


if __name__ == '__main__':
    unittest.main()
