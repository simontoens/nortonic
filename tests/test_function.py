import lang.internal.function as function
import lang.internal.typeinfo as ti
import unittest


class FunctionTest(unittest.TestCase):

    def test_function__no_invocation(self):
        f = function.Function("name")

        self.assertIsNone(f.invocation)

    def test_function__reduce_invcations(self):
        f = function.Function("name")
        f.has_definition = True
        f.register_invocation([ti.TypeInfo.int(), ti.TypeInfo.str()])
        f.register_invocation([ti.TypeInfo.int(), ti.TypeInfo.str()])

        f._reduce_invocations()

        self.assertEqual([ti.TypeInfo.int(), ti.TypeInfo.str()], f.invocation)

    def test_function__reduce_invcations_with_none(self):
        f = function.Function("name")
        f.has_definition = True
        f.register_invocation([ti.TypeInfo.none(), ti.TypeInfo.str()])
        f.register_invocation([ti.TypeInfo.int(), ti.TypeInfo.str()])

        f._reduce_invocations()

        self.assertEqual([ti.TypeInfo.int(), ti.TypeInfo.str()], f.invocation)


if __name__ == '__main__':
    unittest.main()
