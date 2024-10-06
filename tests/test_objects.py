import copy
import util.objects as objects
import unittest


class ListTest(unittest.TestCase):

    def test_strict_read_only(self):
        class A:
            def __init__(self):
                self.i = 1

        a = objects.StrictReadOnly(A())

        self.assertEqual(a.i, 1)
        with self.assertRaises(Exception) as ctx:
            a.i = 2
        self.assertIn("cannot set i", str(ctx.exception))

    def test_copy__read_only(self):
        class A:
            def __init__(self):
                self.i = 1
        a = objects.StrictReadOnly(A())

        a2 = copy.deepcopy(a)

        self.assertIs(a, a2)

    def test_silent_read_only(self):
        class A:
            def __init__(self):
                self.i = 1

        a = objects.SilentReadOnly(A())
        a.i = 2

        self.assertEqual(a.i, 1)

    def test_copy__strict(self):
        class A:
            def __init__(self):
                self.i = 1
        a = objects.SilentReadOnly(A())

        a2 = copy.deepcopy(a)

        self.assertIs(a, a2)


if __name__ == '__main__':
    unittest.main()
