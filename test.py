# def foo(i):
#     print(i)
# foo("foo".index("f"))

def foo(i):
    if i == -1:
        print("not found")
    else:
        print("found")
foo("foo".find("zoo"))
