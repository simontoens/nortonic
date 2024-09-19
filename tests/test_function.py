from lang.internal import Function, TypeInfo
import unittest


class FunctionTest(unittest.TestCase):

    def test_function__no_invocation(self):
        f = Function("name")

        self.assertIsNone(f.invocation)

    def test_function__reduce_invcations(self):
        f = Function("name")
        f.has_definition = True
        f.register_invocation([TypeInfo.int(), TypeInfo.str()])
        f.register_invocation([TypeInfo.int(), TypeInfo.str()])

        f._reduce_invocations()

        self.assertEqual([TypeInfo.int(), TypeInfo.str()], f.invocation)

    def test_function__reduce_invcations_with_none(self):
        f = Function("name")
        f.has_definition = True
        f.register_invocation([TypeInfo.none(), TypeInfo.str()])
        f.register_invocation([TypeInfo.int(), TypeInfo.str()])

        f._reduce_invocations()

        self.assertEqual([TypeInfo.int(), TypeInfo.str()], f.invocation)


if __name__ == '__main__':
    unittest.main()
