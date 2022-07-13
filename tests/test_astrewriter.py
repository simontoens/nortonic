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
        rewriter = self._get_call_node_rewriter(module_node, [context.TypeInfo.str()])

        rewriter.chain_method_call("chained")

        self._t(module_node, 'len("foo").chained()')

    def test_chain_method_with_args(self):
        code = 'len("foo")'
        module_node = astm.parse(code)
        rewriter = self._get_call_node_rewriter(module_node, [context.TypeInfo.str()])

        rewriter.chain_method_call("chained", ["a1", "a2"])

        self._t(module_node, 'len("foo").chained("a1", "a2")')

    def test_chain_multiple_methods_with_args(self):
        code = 'len("foo")'
        module_node = astm.parse(code)
        rewriter = self._get_call_node_rewriter(module_node, [context.TypeInfo.str()])

        rewriter.chain_method_call("m1", ["a1"]).chain_method_call("m2", ["a2"])

        self._t(module_node, 'len("foo").m1("a1").m2("a2")')

    def test_reassign_to_arg(self):
        code = 'sorted(a)'
        module_node = astm.parse(code)
        rewriter = self._get_call_node_rewriter(module_node, [context.TypeInfo.int()])

        rewriter.reassign_to_arg()

        self._t(module_node, 'a = sorted(a)')

    def _get_call_node_rewriter(self, module_node, arg_type_infos=[]):
        ctx = context.ASTContext()
        expr_node = module_node.body[0]
        call_node = expr_node.value
        assert isinstance(call_node, astm.Call), call_node
        assert len(call_node.args) == len(arg_type_infos)
        for i in range(0, len(arg_type_infos)):
            arg_node = call_node.args[i]
            type_info = arg_type_infos[i]
            ctx.register_type_info_by_node(arg_node, type_info)
        return astrewriter.ASTRewriter(call_node, call_node.args, ctx)

    def _t(self, module_node, expected_code):
        ctx = context.ASTContext()
        code = run._emit(module_node, ctx, syntax.PythonSyntax())
        self.assertEqual(expected_code, code)
        

if __name__ == '__main__':
    unittest.main()
