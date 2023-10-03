def f(m):
    f2(m)
def f2(m2):
    m2[1] = "foo"
d = {}
f(d)
print("Dict:", d)
