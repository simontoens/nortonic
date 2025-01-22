class A:
    def __init__(self, name):
        self.name = name
    def greeting(self):
        return "hello, " + self.name
a = A("goo")
a.greeting()
