def get_counter_info(initial_value, increment):
    print("initial value is", initial_value)
    return 0, 1

def get_middle_element(names):
    c_info = get_counter_info(0, 1)
    counter = c_info[0]
    for name in names:
        counter = counter + c_info[1]
        last_element = name
        if counter == len(names) / 2:
            return name
    print("didn't find middle element, last one was", last_element)

middle = get_middle_element(("e1", "e2", "e3", "e4"))
print(middle)
