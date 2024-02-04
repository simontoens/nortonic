def get_counter_info(initial_value, increment):
    print("initial value is", initial_value)
    return 0, 1
def get_middle_element(names):
    c_info = get_counter_info(0, 1)
    counter = c_info[0]
    middle_element_index = None
    for name in names:
        counter = counter + c_info[1]
        last_element = name
        if counter == len(names) / 2:
            middle_element_index = counter
    print("last element processed:", last_element)
    return names[middle_element_index]
el = get_middle_element(("e1", "e2", "e3", "e4"))
print("the element closest to the middle is", el)



#l = [3, 2, 1]
#l.sort()


# ok
# l = []
# l.append("foo")
# s = l[0]


# def f1(l1, o):
#     f2(l1, o)
# def f2(l2, o):
#     f3(l2, o)
# def f3(l3, o):
#     l3.append(1)
#     print("Hello", o)
# l = []
# f1(l, "msg")
# print("List:", l)


# def f(l, d):
#     f2(l, d)

# def f2(l, d):
#     d[2] = "fo2"
#     l.append(2)


# d = {}
# l0 = []
# f(l0, d)
# print("Dict:", d)
# print("List:", l0)
