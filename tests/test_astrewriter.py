from target import targetlanguage
import ast as astm
import astrewriter
import context
import run
import target
import unittest
import visitor
import visitors


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

    def test_chain_method__ident(self):
        code = 'f'
        module_node = astm.parse(code)
        rewriter = self._get_call_node_rewriter(module_node, [context.TypeInfo.str()])

        rewriter.chain_method_call("chained")

        self._t(module_node, "f.chained()")

    def test_reassign_to_arg(self):
        code = 'sorted(a)'
        module_node = astm.parse(code)
        rewriter = self._get_call_node_rewriter(module_node, [context.TypeInfo.int()])

        rewriter.reassign_to_arg()

        self._t(module_node, 'a = sorted(a)')

    def test_rewrite_as_func_call(self):
        code = 'f.readlines()'
        module_node = astm.parse(code)
        rewriter = self._get_call_node_rewriter(module_node)

        rewriter.rewrite_as_func_call()

        self._t(module_node, 'readlines(f)')

    def test_rewrite_as_func_call__then_chain_method(self):
        code = 'f.readlines()'
        module_node = astm.parse(code)
        rewriter = self._get_call_node_rewriter(module_node)

        rewriter.rewrite_as_func_call().chain_method_call("m1")

        self._t(module_node, 'readlines(f).m1()')

    def test_rewrite_as_func_call__then_replace_node(self):
        code = 'f.readlines()'
        module_node = astm.parse(code)
        rewriter = self._get_call_node_rewriter(module_node)

        rewriter.rewrite_as_func_call()
        call = rewriter.call("Arrays.asList")
        rewriter.replace_node_with(call, current_node_becomes_singleton_arg=True)

        self._t(module_node, 'Arrays.asList(readlines(f))')

    def _get_call_node_rewriter(self, module_node, arg_type_infos=[]):
        ctx = context.ASTContext()
        expr_node = module_node.body[0]
        target_node = expr_node.value
        arg_nodes = []
        if isinstance(target_node, astm.Call):
            arg_nodes = target_node.args
            assert len(arg_nodes) == len(arg_type_infos)
            for i in range(0, len(arg_type_infos)):
                arg_node = arg_nodes[i]
                type_info = arg_type_infos[i]
                ctx.register_type_info_by_node(arg_node, type_info)
        elif isinstance(target_node, astm.Name):
            pass
        else:
            assert False, "Unexpected node %s" % target_node
        return astrewriter.ASTRewriter(target_node, arg_nodes, ctx)

    def _t(self, module_node, expected_code):
        ctx = context.ASTContext()
        code = run._emit(module_node, ctx, target.python.PythonSyntax())
        self.assertEqual(expected_code, code)
        

if __name__ == '__main__':
    unittest.main()
