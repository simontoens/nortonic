def get_name():
    a = 1
    if a == 1:
        return "foo"
    else:
        return None

name = get_name()
if name is None:
    print("no name")
else:
    print("got name", name)

