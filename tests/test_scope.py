import ast
import scope as scopem
import unittest


class ScopeTest(unittest.TestCase):

    def test_is_declaration_node(self):
        scope = scopem.Scope(parent_scope=None, ast_node=None)
        ident_node = _get_ident_node("a")
        ident_node2 = _get_ident_node("a")

        scope.register_ident_node(ident_node)
        scope.register_ident_node(ident_node2)        

        self.assertTrue(scope.is_declaration_node(ident_node))
        self.assertFalse(scope.is_declaration_node(ident_node2))

    def test_has_been_declared(self):
        scope = scopem.Scope(parent_scope=None, ast_node=None)
        nested_scope = scopem.Scope(parent_scope=scope, ast_node=None)
        a_ident_node = _get_ident_node("a")
        b_ident_node = _get_ident_node("b")

        scope.register_ident_node(a_ident_node)
        nested_scope.register_ident_node(b_ident_node)

        self.assertTrue(scope.has_been_declared("a"))
        self.assertTrue(nested_scope.has_been_declared("a"))
        self.assertFalse(scope.has_been_declared("b"))
        self.assertTrue(nested_scope.has_been_declared("b"))

    def test_get_ident_nodes_by_name(self):
        scopem._global_ident_node_registry = {} # reset
        scope = scopem.Scope(parent_scope=None, ast_node=None)
        a1_node = _get_ident_node("a")
        a2_node = _get_ident_node("a")
        b_node = _get_ident_node("b")

        scope.register_ident_node(a1_node)
        scope.register_ident_node(a2_node)
        scope.register_ident_node(b_node)

        self.assertEqual(set([a1_node, a2_node]),
                         scope.get_ident_nodes_by_name("a"))
        self.assertEqual(set([b_node]),
                         scope.get_ident_nodes_by_name("b"))


def _get_ident_node(ident_name):
        ident = ast.Name()
        ident.id = ident_name
        return ident


if __name__ == '__main__':
    unittest.main()
