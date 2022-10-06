def get_age_and_fav_num(birthyear):
    this_year = 2022
    return this_year - birthyear, 4

age, num = get_age_and_fav_num(2015)
print("Age is", age, "and favorite number is", num)
