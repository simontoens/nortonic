def f1(f):
    return f()

l = lambda: 2
i = f1(lambda: 2)
print(i)
