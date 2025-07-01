def foo(i):
    return i+1

a = 2
#b = foo(3 if 1 == 1 else 4)
#b = foo(3 if 1 == 1 else 4) if a > 100 else 4
b = 3 if a > 100 else foo(3 if 1 == 1 else 4 if 2==2 else 5) if a == 2 else foo(1 if a > 100 else 2 if a > 200 else a)
#foo(3 if 1 == 1 else 4)


#f = lambda: 3

#n = 2

#lambda: 2 if n==2 else 3

# if n == 200:
#     i = 2 if n == 2 else 3
#     print(i)

