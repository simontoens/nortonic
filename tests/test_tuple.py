from run import run
import ast as astm
import syntax as sy
import unittest


class TupleTest(unittest.TestCase):

    def test_homogeneous_types(self):
        py = "a = 1"
        self._t(syntax=sy.PythonSyntax(), code=py, expected="a = 1")
        self._t(syntax=sy.JavaSyntax(), code=py, expected="Integer a = 1;")
        self._t(syntax=sy.ElispSyntax(), code=py, expected="(setq a 1)")
        
    def _t(self, code, expected, syntax):
        generated_code = run(code, syntax)

        self.assertEqual(expected.strip(), generated_code)


if __name__ == '__main__':
    unittest.main()
    
