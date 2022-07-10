import ast as astm
import astrewriter
import context
import run
import syntax
import unittest


class ASTRewriterTest(unittest.TestCase):

    def test_chain_method(self):
        code = 'len("foo")'
        module_node = astm.parse(code)
        rewriter = self._get_call_node_rewriter(module_node)

        rewriter.chain_method_call("chained")

        self._t(module_node, 'len("foo").chained()')

    def test_chain_method_with_args(self):
        code = 'len("foo")'
        module_node = astm.parse(code)
        rewriter = self._get_call_node_rewriter(module_node)

        rewriter.chain_method_call("chained", ["a1", "a2"])

        self._t(module_node, 'len("foo").chained("a1", "a2")')

    def test_chain_multiple_methods_with_args(self):
        code = 'len("foo")'
        module_node = astm.parse(code)
        rewriter = self._get_call_node_rewriter(module_node)

        rewriter.chain_method_call("m1", ["a1"]).chain_method_call("m2", ["a2"])

        self._t(module_node, 'len("foo").m1("a1").m2("a2")')

    # def test_reassign_to_arg(self):
    #     code = 'sorted(a)'
    #     module_node = astm.parse(code)
    #     rewriter = self._get_call_node_rewriter(module_node)

    #     rewriter.reassign_to_arg()

    #     self._t(module_node, 'a = sorted(a)')

    def _get_call_node_rewriter(self, module_node):
        expr_node = module_node.body[0]
        call_node = expr_node.value
        assert isinstance(call_node, astm.Call), call_node
        args = [syntax.Argument(call_node.args[0], str)]
        ctx = context.ASTContext()
        return astrewriter.ASTRewriter(call_node, args, ctx)

    def _t(self, module_node, expected_code):
        ctx = context.ASTContext()
        code = run._emit(module_node, ctx, syntax.PythonSyntax())
        self.assertEqual(expected_code, code)
        

if __name__ == '__main__':
    unittest.main()
