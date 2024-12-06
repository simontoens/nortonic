import os
import tempfile
import types
import unittest
import util.types


class TypesTest(unittest.TestCase):

    def test_instanceof(self):
        self.assertTrue(util.types.instanceof_py_function(open))
        self.assertTrue(util.types.instanceof_py_function(str))
        self.assertTrue(util.types.instanceof_py_function(str.join))
        self.assertTrue(util.types.instanceof_py_function(print))
        self.assertTrue(util.types.instanceof_py_function(os.path.join))

    def test_name(self):
        self.assertEqual(util.types.get_py_function_name(open), "open")
        self.assertEqual(util.types.get_py_function_name(str.join), "join")
        self.assertEqual(util.types.get_py_function_name(print), "print")
        self.assertEqual(util.types.get_py_function_name(str), "str")
        self.assertEqual(util.types.get_py_function_name(os.path.join), "join")

    def test_get_receiver_type(self):
        self.assertIs(util.types.get_receiver_type(open), None)
        self.assertIs(util.types.get_receiver_type(str.join), str)
        self.assertIs(util.types.get_receiver_type(print), None)
        self.assertIs(util.types.get_receiver_type(str), None)
        self.assertIs(util.types.get_receiver_type(str), None)

    def test_mod_receiver_type(self):
        m = util.types.get_receiver_type(os.path.join)

        self.assertIs(type(m), types.ModuleType)
        self.assertEqual(m.__name__, "os.path")

        m = util.types.get_receiver_type(os.write)

        self.assertIs(type(m), types.ModuleType)
        self.assertEqual(m.__name__, "os")

        self.assertIs(util.types.get_receiver_type(os), None)

        # would be nicer to get os back here:
        self.assertIs(util.types.get_receiver_type(os.path), None)

    def test_get_real_module(self):
        self.assertEqual(util.types.get_real_module(os).__name__, "os")
        self.assertEqual(util.types.get_real_module(os.path).__name__, "os.path")
        self.assertEqual(util.types.get_real_module(tempfile).__name__, "tempfile")


if __name__ == '__main__':
    unittest.main()
