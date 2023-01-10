from context import TypeInfo
import unittest


class TypeInfoTest(unittest.TestCase):

    def test_eq(self):
        self.assertEqual(TypeInfo(int), TypeInfo(int))
        self.assertEqual(TypeInfo(str), TypeInfo(str))
        self.assertNotEqual(TypeInfo(int), TypeInfo(str))
        self.assertEqual(TypeInfo(list), TypeInfo(list))
        self.assertEqual(
            TypeInfo(list).of(TypeInfo(int)),
            TypeInfo(list).of(TypeInfo(int)))
        self.assertNotEqual(
            TypeInfo(list).of(TypeInfo(int)),
            TypeInfo(list).of(TypeInfo(str)))
        self.assertNotEqual(
            TypeInfo(list).of(TypeInfo(int), TypeInfo(int)),
            TypeInfo(list).of(TypeInfo(int)))
        self.assertEqual(
            TypeInfo(list).of(TypeInfo(list)),
            TypeInfo(list).of(TypeInfo(list)))
        self.assertEqual(
            TypeInfo(list).of(TypeInfo(list).of(TypeInfo(int))),
            TypeInfo(list).of(TypeInfo(list).of(TypeInfo(int))))
        self.assertNotEqual(
            TypeInfo(list).of(TypeInfo(list).of(TypeInfo(int))),
            TypeInfo(list).of(TypeInfo(list).of(TypeInfo(str))))

    def test_hash(self):
        self.assertEqual(hash(TypeInfo(int)), hash(TypeInfo(int)))
        self.assertEqual(hash(TypeInfo(str)), hash(TypeInfo(str)))
        self.assertNotEqual(hash(TypeInfo(int)), hash(TypeInfo(str)))
        self.assertEqual(hash(TypeInfo(list)), hash(TypeInfo(list)))
        self.assertEqual(
            hash(TypeInfo(list).of(TypeInfo(int))),
            hash(TypeInfo(list).of(TypeInfo(int))))
        self.assertNotEqual(
            hash(TypeInfo(list).of(TypeInfo(int))),
            hash(TypeInfo(list).of(TypeInfo(str))))
        self.assertNotEqual(
            hash(TypeInfo(list).of(TypeInfo(int), TypeInfo(int))),
            hash(TypeInfo(list).of(TypeInfo(int))))
        self.assertEqual(
            hash(TypeInfo(list).of(TypeInfo(list))),
            hash(TypeInfo(list).of(TypeInfo(list))))
        self.assertEqual(
            hash(TypeInfo(list).of(TypeInfo(list).of(TypeInfo(int)))),
            hash(TypeInfo(list).of(TypeInfo(list).of(TypeInfo(int)))))
        self.assertNotEqual(
            hash(TypeInfo(list).of(TypeInfo(list).of(TypeInfo(int)))),
            hash(TypeInfo(list).of(TypeInfo(list).of(TypeInfo(str)))))

    def test_contains_homogeneous_types__empty(self):
        ti = TypeInfo(list)

        self.assertTrue(ti.contains_homogeneous_types)

    def test_contains_homogeneous_types(self):
        ti = TypeInfo(list)
        ti.register_contained_type(0, TypeInfo.str())
        ti.register_contained_type(1, TypeInfo.str())
        self.assertTrue(ti.contains_homogeneous_types)

        ti = TypeInfo(list)
        ti.register_contained_type(0, TypeInfo.str())
        ti.register_contained_type(1, TypeInfo.int())
        self.assertFalse(ti.contains_homogeneous_types)

        ti = TypeInfo(list)
        ti.register_contained_type(0, TypeInfo.list().of(TypeInfo(str)))
        ti.register_contained_type(1, TypeInfo.list().of(TypeInfo(str)))
        self.assertTrue(ti.contains_homogeneous_types)

        ti = TypeInfo(list)
        ti.register_contained_type(0, TypeInfo.list().of(TypeInfo(str)))
        ti.register_contained_type(1, TypeInfo.list().of(TypeInfo(int)))
        self.assertFalse(ti.contains_homogeneous_types)


if __name__ == '__main__':
    unittest.main()
