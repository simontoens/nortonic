def foo(d2):
    return d2[1]

d = {}
d[1] = foo(d)
d[foo(d)] = 2


