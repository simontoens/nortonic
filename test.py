def foo():
    return None

d = {foo(): foo()}
d[None] = None
d[foo()] = foo()
d[2] = 3
