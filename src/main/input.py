class A:
    def __init__(self, n):
        self.name = n
    def greeting(self):
        return "hello, " + self.name
def printGreeting(a):
    gr = a.greeting()
    print(gr)
a = A("foo")
printGreeting(a)
