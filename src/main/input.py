class A:
    def __init__(self, n):
        self.name = n
    def greeting(self):
        return "hello, " + self.name
a = A("foo")
gr = A("goo").greeting()
print(gr)




# def foo0(s):
#     return s, "f"

# def foo():
#     return "s", "f"

# a,b = foo()
# c,d = foo0("s")
