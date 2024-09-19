import unittest
import visitor.context as context


class ContextTest(unittest.TestCase):

    def test_astcontext__get_unique_identifier_name(self):
        ctx = context.ASTContext()
        ctx.register_ident_names(["t0", "t1"])

        self.assertEqual("t", ctx.get_unique_identifier_name())
        self.assertEqual("t2", ctx.get_unique_identifier_name())
        self.assertEqual("t3", ctx.get_unique_identifier_name())


if __name__ == '__main__':
    unittest.main()
