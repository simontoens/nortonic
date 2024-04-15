from context import ASTContext, Function, TypeInfo
import unittest


class ContextTest(unittest.TestCase):

    def test_function__no_invocation(self):
        f = Function("name")

        self.assertIsNone(f.invocation)

    def test_function__reduce_invcations(self):
        f = Function("name")
        f.register_invocation([TypeInfo.int(), TypeInfo.str()])
        f.register_invocation([TypeInfo.int(), TypeInfo.str()])

        f._reduce_invocations()

        self.assertEqual([TypeInfo.int(), TypeInfo.str()], f.invocation)

    def test_function__reduce_invcations_with_none(self):
        f = Function("name")
        f.register_invocation([TypeInfo.none(), TypeInfo.str()])
        f.register_invocation([TypeInfo.int(), TypeInfo.str()])

        f._reduce_invocations()

        self.assertEqual([TypeInfo.int(), TypeInfo.str()], f.invocation)

    def test_astcontext__get_unique_identifier_name(self):
        ctx = ASTContext()
        ctx.register_ident_names(["t0", "t1"])

        self.assertEqual("t", ctx.get_unique_identifier_name())
        self.assertEqual("t2", ctx.get_unique_identifier_name())
        self.assertEqual("t3", ctx.get_unique_identifier_name())


if __name__ == '__main__':
    unittest.main()
