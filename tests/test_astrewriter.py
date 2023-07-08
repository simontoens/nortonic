from context import TypeInfo
from target import targetlanguage
import ast as astm
import astrewriter
import context
import run
import target
import unittest


class ASTRewriterTest(unittest.TestCase):

    def setUp(self):
        run._setup()

    def test_chain_method(self):
        code = 'len("foo")'
        module_node = astm.parse(code)
        rewriter = self._get_call_node_rewriter(
            module_node, rtn_type=TypeInfo.int(),
            arg_types=[context.TypeInfo.str()])

        rewriter.chain_method_call("chained")

        self._t(module_node, 'len("foo").chained()')

    def test_chain_method_with_args(self):
        code = 'len("foo")'
        module_node = astm.parse(code)
        rewriter = self._get_call_node_rewriter(
            module_node, rtn_type=TypeInfo.int(),
            arg_types=[context.TypeInfo.str()])

        rewriter.chain_method_call("chained", ["a1", "a2"])

        self._t(module_node, 'len("foo").chained("a1", "a2")')

    def test_chain_multiple_methods_with_args(self):
        code = 'len("foo")'
        module_node = astm.parse(code)
        rewriter = self._get_call_node_rewriter(
            module_node, rtn_type=TypeInfo.int(),
            arg_types=[context.TypeInfo.str()])

        rewriter.chain_method_call("m1", ["a1"]).chain_method_call("m2", ["a2"])

        self._t(module_node, 'len("foo").m1("a1").m2("a2")')

    def test_chain_method__ident(self):
        code = 'f'
        module_node = astm.parse(code)
        rewriter = self._get_call_node_rewriter(module_node, TypeInfo.int())

        rewriter.chain_method_call("chained")

        self._t(module_node, "f.chained()")

    def test_reassign_to_arg(self):
        code = 'sorted(a)'
        module_node = astm.parse(code)
        rewriter = self._get_call_node_rewriter(
            module_node, rtn_type=TypeInfo.list(),
            arg_types=[context.TypeInfo.int()])

        rewriter.reassign_to_arg()

        self._t(module_node, 'a = sorted(a)')

    def test_rewrite_as_func_call(self):
        code = 'f.readlines()'
        module_node = astm.parse(code)
        rewriter = self._get_call_node_rewriter(module_node, TypeInfo.list())

        rewriter.rewrite_as_func_call()

        self._t(module_node, 'readlines(f)')

    def test_rewrite_as_func_call__then_chain_method(self):
        code = 'f.readlines()'
        module_node = astm.parse(code)
        rewriter = self._get_call_node_rewriter(module_node, TypeInfo.list())

        rewriter.rewrite_as_func_call().chain_method_call("m1")

        self._t(module_node, 'readlines(f).m1()')

    def test_rewrite_as_func_call__then_replace_node(self):
        code = 'f.readlines()'
        module_node = astm.parse(code)
        rewriter = self._get_call_node_rewriter(module_node, TypeInfo.list())

        rewriter.rewrite_as_func_call()
        call = rewriter.call("Arrays.asList")
        rewriter.replace_node_with(call, current_node_becomes_singleton_arg=True)

        self._t(module_node, 'Arrays.asList(readlines(f))')

    def test_rewrite_for_loop_as_c_style_loop(self):
        code = """
for i in range(0, 20):
    print(i)
"""
        expected_code = """
for i = 0; i < 20; i += 1:
    print(i)
"""
        module_node = astm.parse(code)
        rewriter = self._get_for_node_rewriter(module_node)

        rewriter.rewrite_as_c_style_loop()

        self._t(module_node, expected_code)

    def test_rewrite_if_exp_as_if_stmt__assign(self):
        code = """
a = 3 if 0 == 0 else 2
"""

        expected_code = """
if 0 == 0:
    a = 3
else:
    a = 2
"""
        module_node = astm.parse(code)
        rewriter = self._get_if_exp_node_rewriter(module_node)

        rewriter.rewrite_as_if_stmt()

        self._t(module_node, expected_code)

    def test_rewrite_if_exp_as_if_stmt__return(self):
        code = """
return 3 if 0 == 0 else 2
"""

        expected_code = """
if 0 == 0:
    return 3
else:
    return 2
"""
        module_node = astm.parse(code)
        rewriter = self._get_if_exp_node_rewriter(module_node)

        rewriter.rewrite_as_if_stmt()

        self._t(module_node, expected_code)

    def _get_for_node_rewriter(self, module_node):
        ctx = context.ASTContext()
        for_node = module_node.body[0]
        assert isinstance(for_node, astm.For)
        ctx.register_type_info_by_node(for_node, context.TypeInfo.notype())
        ctx.register_type_info_by_node(for_node.target, context.TypeInfo.int())
        arg_nodes=[for_node.target, for_node.iter]
        return astrewriter.ASTRewriter(for_node, arg_nodes, ctx,
                                       parent_body=[])

    def _get_if_exp_node_rewriter(self, module_node):
        ctx = context.ASTContext()
        context_node = module_node.body[0]
        if_exp_parent_node = context_node
        if_exp_node = context_node.value
        assert isinstance(if_exp_node, astm.IfExp)
        # a = 3 if 0 == 0 else 2
        # body: 3 <Constant Node>
        # test: 0 == 0 <Compare Node>
        # orelse: 2 <Constant Node>
        # if_expr_parent_node: a = <IfExp Node>
        arg_nodes=(if_exp_node.body,
                   if_exp_node.test,
                   if_exp_node.orelse,
                   if_exp_parent_node)
        return astrewriter.ASTRewriter(if_exp_node, arg_nodes, ctx,
                                       parent_body=[])

    def _get_call_node_rewriter(self, module_node, rtn_type, arg_types=[]):
        ctx = context.ASTContext()
        expr_node = module_node.body[0]
        target_node = expr_node.value
        ctx.register_type_info_by_node(target_node, rtn_type)
        arg_nodes = []
        if isinstance(target_node, astm.Call):
            arg_nodes = target_node.args
            assert len(arg_nodes) == len(arg_types)
            for i in range(0, len(arg_types)):
                arg_node = arg_nodes[i]
                type_info = arg_types[i]
                ctx.register_type_info_by_node(arg_node, type_info)
        elif isinstance(target_node, astm.Name):
            pass
        else:
            assert False, "Unexpected node %s" % target_node
        return astrewriter.ASTRewriter(target_node, arg_nodes, ctx,
                                       parent_body=[])

    def _t(self, module_node, expected_code):
        ctx = context.ASTContext()
        code = run._emit(module_node, ctx, target.python.PythonSyntax())
        self.assertEqual(expected_code.strip(), code.strip())
        

if __name__ == '__main__':
    unittest.main()
