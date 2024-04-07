def foo(mf):
    return mf()

f = None
f = lambda: 1
foo(f)


