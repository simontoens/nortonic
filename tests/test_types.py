import unittest
import util.types


class TypesTest(unittest.TestCase):

    def test_instanceof(self):
        self.assertTrue(util.types.instanceof_py_function(str))
        self.assertTrue(util.types.instanceof_py_function(str.join))
        self.assertTrue(util.types.instanceof_py_function(print))

    def test_name(self):
        self.assertEqual("join", util.types.get_py_function_name(str.join))
        self.assertEqual("print", util.types.get_py_function_name(print))
        self.assertEqual("str", util.types.get_py_function_name(str))

    def test_get_receiver_type(self):
        self.assertIs(util.types.get_receiver_type(str.join), str)
        self.assertIs(util.types.get_receiver_type(print), None)
        self.assertIs(util.types.get_receiver_type(str), None)


if __name__ == '__main__':
    unittest.main()
