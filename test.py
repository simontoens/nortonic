def print_first_element(l):
    l.append(10)
    el1 = l[0]
    print("the first element is", el1)
    return [10, 11, 12]

lst = print_first_element([1,2])
print("all element are", lst)
