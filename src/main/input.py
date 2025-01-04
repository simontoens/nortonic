class A:
    def __init__(self, name):
        self.name = name
    def greeting(self):
        return "hello " + self.name
    def get_name(self):
        return self.name
a = A("world")
print(a.greeting())
