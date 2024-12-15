class Test:
    def __init__(self, name):
        print("ok")
    def hello(self, name):
        return "hello, " + name

f1(Test("fff"))

def f1(s):
    s.hello("f")
    #test.hello("joe")
