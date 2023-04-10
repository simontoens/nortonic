import context
import unittest


class ContextTest(unittest.TestCase):

    def test_get_unique_identifier_name(self):
        ctx = context.ASTContext()
        ctx.register_ident_names(["t0", "t1"])

        self.assertEqual("t", ctx.get_unqiue_identifier_name())
        self.assertEqual("t2", ctx.get_unqiue_identifier_name())
        self.assertEqual("t3", ctx.get_unqiue_identifier_name())


if __name__ == '__main__':
    unittest.main()
