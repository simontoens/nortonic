import lang.internal.function as function
import lang.internal.typeinfo as typeinfo
import visitor.attrresolver as res
import unittest


class AttributeResolverTest(unittest.TestCase):

    def setUp(self):
        self.default_resolver = res.AttributeResolver()
        self.resolver = res.AttributeResolver(self.default_resolver)

    def test_easy(self):
        m = typeinfo.TypeInfo.module("m")
        f = function.Function("f", typeinfo.TypeInfo.int())

        self.resolver.register(m, f)

        self.assertIs(self.resolver.resolve_to_function(m, "f"), f)

    def test_resolve_no_match(self):
        join = function.Function("join", typeinfo.TypeInfo.int())
        self.resolver.register(typeinfo.TypeInfo.module("os.path"), join)
        mti = typeinfo.TypeInfo.module("bad.module")

        self.assertIsNone(self.resolver.resolve_to_function(mti, "path"))
        self.assertIsNone(self.resolver.resolve_to_type(mti, "path"))

    def test_resolve_exact_match(self):
        join = function.Function("join", typeinfo.TypeInfo.int())
        self.resolver.register(typeinfo.TypeInfo.module("os.path"), join)
        mti = typeinfo.TypeInfo.module("os.path")

        f = self.resolver.resolve_to_function(mti, "join")
        ti = self.resolver.resolve_to_type(mti, "join")

        self.assertIs(f, join)
        self.assertEqual(ti, typeinfo.TypeInfo.int())

    def test_multiple_functions(self):
        join = function.Function("join", typeinfo.TypeInfo.int())
        self.resolver.register(typeinfo.TypeInfo.module("os.path"), join)
        relative = function.Function("relative", typeinfo.TypeInfo.str())
        self.resolver.register(typeinfo.TypeInfo.module("os.path"), relative)
        mti = typeinfo.TypeInfo.module("os.path")

        self.assertIs(self.resolver.resolve_to_function(mti, "join"), join)
        self.assertIs(self.resolver.resolve_to_function(mti, "relative"), relative)

    def test_resolve_module_then_function(self):
        join = function.Function("join", typeinfo.TypeInfo.int())
        self.resolver.register(typeinfo.TypeInfo.module("os.path"), join)

        os_ti = typeinfo.TypeInfo.module("os")
        path_ti = self.resolver.resolve_to_type(os_ti, "path")
        self.assertEqual(path_ti.name, "os.path")

        ti = self.resolver.resolve_to_type(path_ti, "join")
        self.assertEqual(ti, typeinfo.TypeInfo.int())

    def test_resolve_module_then_attr(self):
        org_mod_ti = typeinfo.TypeInfo.module("os.path.file")
        self.resolver.register(org_mod_ti, "sep", typeinfo.TypeInfo.str())

        os_mod_ti = typeinfo.TypeInfo.module("os")
        path_mod_ti = self.resolver.resolve_to_type(os_mod_ti, "path")
        self.assertEqual(path_mod_ti.name, "os.path")

        file_mod_ti = self.resolver.resolve_to_type(path_mod_ti, "file")
        self.assertEqual(file_mod_ti.name, "os.path.file")

        self.assertEqual(self.resolver.resolve_to_type(file_mod_ti, "sep"),
                         typeinfo.TypeInfo.str())
        self.assertEqual(self.resolver.resolve_to_type(org_mod_ti, "sep"),
                         typeinfo.TypeInfo.str())

    def test_str_type(self):
        upper = function.Function("upper", typeinfo.TypeInfo.str())
        self.resolver.register(typeinfo.TypeInfo.str(), upper)

        self.assertEqual(
            self.resolver.resolve_to_type(typeinfo.TypeInfo.str(), "upper"),
            typeinfo.TypeInfo.str())

    def test_custom_type(self):
        f = function.Function("f", typeinfo.TypeInfo.int())
        self.resolver.register(typeinfo.TypeInfo.clazz("foo"), f)

        self.assertEqual(
            self.resolver.resolve_to_type(typeinfo.TypeInfo.clazz("foo"), "f"),
            typeinfo.TypeInfo.int())

    def test_get_top_level_functions(self):
        f1 = function.Function("f1", typeinfo.TypeInfo.str())
        f2 = function.Function("f2", typeinfo.TypeInfo.int())
        self.resolver.register(res.NO_MODULE, f1)
        self.resolver.register(res.NO_MODULE, f2)

        functions = self.resolver.get_top_level_functions()

        self.assertEqual(2, len(functions))
        self.assertIn(f1, functions)
        self.assertIn(f2, functions)

    def test_resolve_from_default_resolver(self):
        path_mod = typeinfo.TypeInfo.module("os.path")
        join = function.Function("join", typeinfo.TypeInfo.int())
        self.default_resolver.register(path_mod, join) # reg w/ default resolver

        self.assertIs(self.resolver.resolve_to_function(path_mod, "join"), join)
        self.assertEqual(self.resolver.resolve_to_type(path_mod, "join"), typeinfo.TypeInfo.int())


if __name__ == '__main__':
    unittest.main()
