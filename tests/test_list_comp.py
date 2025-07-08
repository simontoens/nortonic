import compilertest
import unittest


class ListCompTest(compilertest.CompilerTest):

    def test_list_comp(self):
        py = """
l = [1, 2, 3]
a = [i for i in l]
"""
        self.py(py, expected=py)


if __name__ == '__main__':
    unittest.main()
