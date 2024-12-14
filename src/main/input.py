class Test:
    def __init__(self, name):
        print("ok")
    def hello(self, name):
        return "hello, " + name

t = Test("wo")
msg = t.hello("man")
print(msg)

