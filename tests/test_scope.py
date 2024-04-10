import ast
import scope as scopem
import unittest


class ScopeTest(unittest.TestCase):

    def test_has_parent(self):
        scope = scopem.Scope(parent_scope=None, ast_node=ast.Name(), namespace=None)
        self.assertFalse(scope.has_parent)
        
        nested_scope = scopem.Scope(parent_scope=scope, ast_node=ast.Name(), namespace=None)
        self.assertTrue(nested_scope.has_parent)

    def test_is_declaration_node(self):
        scope = scopem.Scope(parent_scope=None, ast_node=ast.Name(), namespace=None)
        ident_node = _get_ident_node("a")
        ident_node2 = _get_ident_node("a")

        scope.register_ident_node(ident_node)
        scope.register_ident_node(ident_node2)        

        self.assertTrue(scope.is_declaration_node(ident_node))
        self.assertFalse(scope.is_declaration_node(ident_node2))

    def test_has_been_declared(self):
        scope = scopem.Scope(parent_scope=None, ast_node=ast.Name(), namespace=None)
        nested_scope = scopem.Scope(parent_scope=scope, ast_node=ast.Name(), namespace=None)
        a_ident_node = _get_ident_node("a")
        b_ident_node = _get_ident_node("b")

        scope.register_ident_node(a_ident_node)
        nested_scope.register_ident_node(b_ident_node)

        self.assertTrue(scope.has_been_declared("a"))
        self.assertTrue(nested_scope.has_been_declared("a"))
        self.assertFalse(scope.has_been_declared("b"))
        self.assertTrue(nested_scope.has_been_declared("b"))

    def test_get_declaration_node(self):
        scope = scopem.Scope(parent_scope=None, ast_node=ast.Name(), namespace=None)
        nested_scope = scopem.Scope(parent_scope=scope, ast_node=ast.Name(), namespace=None)
        a_ident_node = _get_ident_node("a")
        b_ident_node = _get_ident_node("b")

        scope.register_ident_node(a_ident_node)
        nested_scope.register_ident_node(b_ident_node)

        self.assertIs(a_ident_node, scope.get_declaration_node("a"))
        self.assertIs(a_ident_node, nested_scope.get_declaration_node("a"))
        self.assertIsNone(scope.get_declaration_node("b"))
        self.assertIs(b_ident_node, nested_scope.get_declaration_node("b"))

    def test_get_identifiers_in_this_scope(self):
        scope = scopem.Scope(parent_scope=None, ast_node=ast.Name(), namespace=None)
        nested_scope = scopem.Scope(parent_scope=scope, ast_node=ast.Name(), namespace=None)

        scope.register_ident_node(_get_ident_node("a"))
        nested_scope.register_ident_node(_get_ident_node("a"))
        nested_scope.register_ident_node(_get_ident_node("c"))

        self.assertEqual(set("a"), scope.get_identifiers_in_this_scope())
        self.assertEqual(set(["a", "c"]), nested_scope.get_identifiers_in_this_scope())

    def test_get_enclosing_namespace(self):
        scope = scopem.Scope(parent_scope=None, ast_node=ast.Name(), namespace="ns1")
        self.assertEqual("ns1", scope.get_enclosing_namespace()[0])

        nested_scope = scopem.Scope(parent_scope=scope, ast_node=ast.Name(), namespace=None)
        self.assertEqual("ns1", nested_scope.get_enclosing_namespace()[0])

        even_more_nested_scope = scopem.Scope(parent_scope=nested_scope, ast_node=ast.Name(), namespace="ns2")
        self.assertEqual("ns2", even_more_nested_scope.get_enclosing_namespace()[0])

    def test_get_identifier_nodes_in_this_scope(self):
        scope = scopem.Scope(parent_scope=None, ast_node=ast.Name(), namespace=None)
        a1_node = _get_ident_node("a")
        a2_node = _get_ident_node("a")
        b_node = _get_ident_node("b")

        scope.register_ident_node(a1_node)
        scope.register_ident_node(a2_node)
        scope.register_ident_node(b_node)

        self.assertEqual(set([a1_node, a2_node]),
                         scope.get_identifier_nodes_in_this_scope("a"))
        self.assertEqual(set([b_node]),
                         scope.get_identifier_nodes_in_this_scope("b"))


def _get_ident_node(ident_name):
        ident = ast.Name()
        ident.id = ident_name
        return ident


if __name__ == '__main__':
    unittest.main()
