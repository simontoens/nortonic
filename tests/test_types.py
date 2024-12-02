import os
import types
import unittest
import util.types


class TypesTest(unittest.TestCase):

    def test_instanceof(self):
        self.assertTrue(util.types.instanceof_py_function(str))
        self.assertTrue(util.types.instanceof_py_function(str.join))
        self.assertTrue(util.types.instanceof_py_function(print))
        self.assertTrue(util.types.instanceof_py_function(os.path.join))

    def test_name(self):
        self.assertEqual(util.types.get_py_function_name(str.join), "join")
        self.assertEqual(util.types.get_py_function_name(print), "print")
        self.assertEqual(util.types.get_py_function_name(str), "str")
        self.assertEqual(util.types.get_py_function_name(os.path.join), "join")

    def test_get_receiver_type(self):
        self.assertIs(util.types.get_receiver_type(str.join), str)
        self.assertIs(util.types.get_receiver_type(print), None)
        self.assertIs(util.types.get_receiver_type(str), None)
        self.assertIs(util.types.get_receiver_type(str), None)

    def test_mod_receiver_type(self):
        m = util.types.get_receiver_type(os.path.join)
        
        self.assertIs(type(m), types.ModuleType)
        self.assertEqual(m.__name__, "os.path")


if __name__ == '__main__':
    unittest.main()
