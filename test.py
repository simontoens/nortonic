import ast

code = """
a = 1
b = 2
"""
node = ast.parse(code)


class RewriteName(ast.NodeTransformer):

    def generic_visit(self, node):
        super().generic_visit(node)
        if isinstance(node, ast.Constant) and node.value == 1:
            return ast.Constant(value=101)
        elif isinstance(node, ast.Assign):
            if node.value.value == 2:
                return None
            else:
                n = ast.Call()
                n.func = ast.Name(id="f1")
                n.args = [node.value]
                n.keywords = []
                return n
        return node


node = RewriteName().visit(node)

print(ast.unparse(node))


#l = [3, 2, 1]
#l.sort()


# ok
# l = []
# l.append("foo")
# s = l[0]


# def f1(l1, o):
#     f2(l1, o)
# def f2(l2, o):
#     f3(l2, o)
# def f3(l3, o):
#     l3.append(1)
#     print("Hello", o)
# l = []
# f1(l, "msg")
# print("List:", l)


# def f(l, d):
#     f2(l, d)

# def f2(l, d):
#     d[2] = "fo2"
#     l.append(2)


# d = {}
# l0 = []
# f(l0, d)
# print("Dict:", d)
# print("List:", l0)
