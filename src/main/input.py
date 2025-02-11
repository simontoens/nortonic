class A:
    def __init__(self, n):
        self.name = n
    def greeting2(self):
        return "hello, " + self.name

def greet(b):
    return b.greeting2()

a = A("foo")
gr = greet(a)
print(gr)
