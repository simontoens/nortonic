import api.bonsaibuilder as builder
import lang.compiler as compiler
import lang.target.python as python
import unittest


class BonsaiBuildlerTest(unittest.TestCase):

    def test_func_call(self):
        bb = builder.BonsaiBuilder()        

        node = (
            bb.function("foo")
                .add_argument(1)
                .add_argument("test")
                .add_argument(bb.function("blah")
                    .add_argument(True))
                .add_argument(bb.identifier("name"))
                .node
        )

        self._t(node, 'foo(1, "test", blah(True), name)')

    def test_func_with_chained_method_call(self):
        bb = builder.BonsaiBuilder()

        node = (
            bb.function("foo")
                .call_method("blah")
                    .add_argument("test")
            .node)

        self._t(node, 'foo().blah("test")')

    def test_ident_with_method_call(self):
        bb = builder.BonsaiBuilder()

        node = bb.identifier("foo").call_method("blah").node

        self._t(node, "foo.blah()")

    def test_with_existing_node(self):
        bb = builder.BonsaiBuilder()
        node = bb.function("foo").node
        bb = builder.BonsaiBuilder()

        node = bb.with_node(node).add_argument(1).node

        self._t(node, "foo(1)")

    def test_rename_func(self):
        bb = builder.BonsaiBuilder()        

        node = bb.function("foo").rename("blah").add_argument(1).node

        self._t(node, "blah(1)")

    def test_rename_method(self):
        bb = builder.BonsaiBuilder()        

        node = (
            bb.identifier("i")
                .call_method("len")
                .rename("len2")
                .add_argument("a")
            .node)

        self._t(node, 'i.len2("a")')

    def test_rename_ident(self):
        bb = builder.BonsaiBuilder()

        node = bb.identifier("foo").rename("blah").call_method("foo").node

        self._t(node, "blah.foo()")

    def test_build_java_streams_expression(self):
        bb = builder.BonsaiBuilder()

        node = (bb.identifier("fruits")
                    .call_method("map")
                        .add_argument(bb.identifier("String::toUpperCase"))
                    .call_method("collect")
                        .add_argument(bb.function("Collectors.toList"))
                .node)

        self._t(node, "fruits.map(String::toUpperCase).collect(Collectors.toList())")

    def test_to_func_call(self):
        bb = builder.BonsaiBuilder()

        node = (bb.identifier("l")
                    .call_method("len")
                    .refactor_to_function_call()
                    .rename("len2")
                    .add_argument(2)
                .node)

        self._t(node, "len2(l, 2)")

    def test_to_method_call(self):
        bb = builder.BonsaiBuilder()

        node = (bb.function("len")
                    .add_argument(bb.identifier("l"))
               .refactor_to_method_call()
               .node)

        self._t(node, "l.len()")


    def _t(self, node, expected_code):
        code = compiler._emit(node, compiler._init(), python.PythonSyntax())
        self.assertEqual(expected_code.strip(), code.strip())


if __name__ == '__main__':
    unittest.main()
    
