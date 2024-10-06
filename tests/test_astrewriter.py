import ast as astm
import lang.astrewriter as astrewriter
import lang.compiler as compiler
import lang.internal.function as func
import lang.internal.typeinfo as ti
import lang.target.python as python
import lang.target.targetlanguage as targetlanguage
import unittest
import visitor.context as context
import visitor.nodeattrs as nodeattrs


class ASTRewriterTest(unittest.TestCase):

    def setUp(self):
        self.ctx = context.ASTContext()        
        compiler._bootstrap(self.ctx)

    def test_chain_method(self):
        code = 'len("foo")'
        module_node = astm.parse(code)
        rewriter = self._get_call_node_rewriter(
            module_node, rtn_type=ti.TypeInfo.int(),
            arg_types=[ti.TypeInfo.str()])
        self.assertIsInstance(rewriter.node, astm.Call)

        rewriter.chain_method_call("chained")

        self._t(module_node, 'len("foo").chained()')
        self.assertIsInstance(rewriter.node.get(), astm.Call)

    def test_chain_method_with_args(self):
        code = 'len("foo")'
        module_node = astm.parse(code)
        rw = self._get_call_node_rewriter(
            module_node, rtn_type=ti.TypeInfo.int(),
            arg_types=[ti.TypeInfo.str()])
        self.assertIsInstance(rw.node, astm.Call)

        rw.chain_method_call("chained").append_args(["a1", "a2"])

        self._t(module_node, 'len("foo").chained("a1", "a2")')
        self.assertIsInstance(rw.node.get(), astm.Call)

    def test_chain_multiple_methods_with_args(self):
        code = 'len("foo")'
        module_node = astm.parse(code)
        rewriter = self._get_call_node_rewriter(
            module_node, rtn_type=ti.TypeInfo.int(),
            arg_types=[ti.TypeInfo.str()])
        self.assertIsInstance(rewriter.node, astm.Call)

        rewriter.\
            chain_method_call("m1").append_args(("a1", "a2")).\
            chain_method_call("m2").append_arg("a10")

        self._t(module_node, 'len("foo").m1("a1", "a2").m2("a10")')

    def test_chain_method__ident(self):
        code = 'f'
        module_node = astm.parse(code)
        rewriter = self._get_call_node_rewriter(module_node, ti.TypeInfo.int())
        self.assertIsInstance(rewriter.node, astm.Name)

        rewriter = rewriter.chain_method_call("chained")

        self._t(module_node, "f.chained()")
        self.assertIsInstance(rewriter.node.get(), astm.Call)

    def test_chain_method_with_args__ident(self):
        code = 'f'
        module_node = astm.parse(code)
        rw = self._get_call_node_rewriter(module_node, ti.TypeInfo.int())
        self.assertIsInstance(rw.node, astm.Name)

        rw.chain_method_call("chained").append_arg("a1").append_arg(rw.ident("i1"))

        self._t(module_node, 'f.chained("a1", i1)')
        self.assertIsInstance(rw.node.get(), astm.Call)

    def test_reassign_to_arg(self):
        code = 'sorted(a)'
        module_node = astm.parse(code)
        rewriter = self._get_call_node_rewriter(
            module_node, rtn_type=ti.TypeInfo.list(),
            arg_types=[ti.TypeInfo.int()])

        rewriter.reassign_to_arg()

        self._t(module_node, 'a = sorted(a)')

    def test_rewrite_as_func_call(self):
        code = 'f.readlines()'
        module_node = astm.parse(code)
        rewriter = self._get_call_node_rewriter(module_node, ti.TypeInfo.list())

        rewriter.rewrite_as_func_call()

        self._t(module_node, 'readlines(f)')

    def test_rewrite_as_func_call__then_chain_method(self):
        code = 'f.readlines()'
        module_node = astm.parse(code)
        rewriter = self._get_call_node_rewriter(module_node, ti.TypeInfo.list())

        rewriter.rewrite_as_func_call().chain_method_call("m1")

        self._t(module_node, 'readlines(f).m1()')

    def test_rewrite_as_func_call__then_replace_node(self):
        code = 'f.readlines()'
        module_node = astm.parse(code)
        rewriter = self._get_call_node_rewriter(module_node, ti.TypeInfo.list())

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
        for_node = module_node.body[0]
        assert isinstance(for_node, astm.For)
        self.ctx.register_type_info_by_node(for_node, ti.TypeInfo.notype())
        self.ctx.register_type_info_by_node(for_node.target, ti.TypeInfo.int())
        arg_nodes=[for_node.target, for_node.iter]
        return astrewriter.ASTRewriter(for_node, arg_nodes, self.ctx,
                                       parent_body=[])

    def _get_if_exp_node_rewriter(self, module_node):
        context_node = module_node.body[0]
        # we just need some type registered
        self.ctx.register_type_info_by_node(context_node, ti.TypeInfo.notype())
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
        return astrewriter.ASTRewriter(if_exp_node, arg_nodes, self.ctx,
                                       parent_body=[])

    def _get_call_node_rewriter(self, module_node, rtn_type, arg_types=[]):
        expr_node = module_node.body[0]
        target_node = expr_node.value
        self.ctx.register_type_info_by_node(target_node, rtn_type)
        arg_nodes = []
        if isinstance(target_node, astm.Call):
            nodeattrs.set_function(target_node, func.Function("test_func_name"))
            arg_nodes = target_node.args
            assert len(arg_nodes) == len(arg_types)
            for i in range(0, len(arg_types)):
                arg_node = arg_nodes[i]
                type_info = arg_types[i]
                self.ctx.register_type_info_by_node(arg_node, type_info)
        elif isinstance(target_node, astm.Name):
            pass
        else:
            assert False, "Unexpected node %s" % target_node
        return astrewriter.ASTRewriter(target_node, arg_nodes, self.ctx,
                                       parent_body=[])

    def _t(self, module_node, expected_code):
        code = compiler._emit(module_node, self.ctx, python.PythonSyntax())
        self.assertEqual(expected_code.strip(), code.strip())
        

if __name__ == '__main__':
    unittest.main()
