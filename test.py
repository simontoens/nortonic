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
