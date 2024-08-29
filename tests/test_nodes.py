from lang import internal
import lang.nodebuilder as nodebuilder
import lang.nodes as nodes
import unittest
import visitor.context as context


class NodesTest(unittest.TestCase):

    def test_shallow_copy_node(self):
        node = nodebuilder.assignment("a", 3)
        ast_context = context.ASTContext()
        assignment_node_ti = internal.TypeInfo.int()
        ast_context.register_type_info_by_node(node, assignment_node_ti)
        lhs_ti = internal.TypeInfo.int()
        ast_context.register_type_info_by_node(node.targets[0], lhs_ti)
        rhs_node_ti = internal.TypeInfo.int()
        ast_context.register_type_info_by_node(node.value, rhs_node_ti)

        copy = nodes.shallow_copy_node(node, ast_context)

        # only the root node is copied
        self.assertIsNot(node, copy)
        self.assertIs(node.targets[0], copy.targets[0])
        self.assertIs(node.value, copy.value)
        # copied root node has associated TypeInfo
        self.assertIsNotNone(ast_context.get_type_info_by_node(copy))
        # the associated TypeInfo instance has not been copied
        self.assertIs(ast_context.get_type_info_by_node(node),
                      ast_context.get_type_info_by_node(copy))

    def test_deep_copy_node(self):
        node = nodebuilder.assignment("a", 3)
        ast_context = context.ASTContext()
        assignment_node_ti = internal.TypeInfo.int()
        ast_context.register_type_info_by_node(node, assignment_node_ti)
        lhs_ti = internal.TypeInfo.int()
        ast_context.register_type_info_by_node(node.targets[0], lhs_ti)
        rhs_node_ti = internal.TypeInfo.int()
        ast_context.register_type_info_by_node(node.value, rhs_node_ti)

        copy = nodes.deep_copy_node(node, ast_context)

        # all nodes are copied
        self.assertIsNot(node, copy)
        self.assertIsNot(node.targets[0], copy.targets[0])
        self.assertIsNot(node.value, copy.value)
        self.assertEqual(node.targets[0].id, copy.targets[0].id)
        self.assertEqual(node.value.value, node.value.value)
        # copied nodes have associated TypeInfo instances
        self.assertIsNotNone(ast_context.get_type_info_by_node(copy))
        self.assertIsNotNone(ast_context.get_type_info_by_node(copy.targets[0]))
        self.assertIsNotNone(ast_context.get_type_info_by_node(copy.value))
        # associated TypeInfo instances are not copied
        self.assertIs(ast_context.get_type_info_by_node(node),
                      ast_context.get_type_info_by_node(copy))
        self.assertIs(ast_context.get_type_info_by_node(node.targets[0]),
                      ast_context.get_type_info_by_node(copy.targets[0]))
        self.assertIs(ast_context.get_type_info_by_node(node.value),
                      ast_context.get_type_info_by_node(copy.value))


    def test_deep_copy_node_twice(self):
        ast_context = context.ASTContext()
        node = nodebuilder.assignment("a", 3)
        ast_context.register_type_info_by_node(node, internal.TypeInfo.int())

        c1 = nodes.deep_copy_node(node, ast_context)
        c2 = nodes.deep_copy_node(node, ast_context)

        self.assertIsNot(c1, c2)
        

if __name__ == '__main__':
    unittest.main()
