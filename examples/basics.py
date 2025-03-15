# Types and function calls.
#
# Only a small set of builtin functions are currently supported.
# Adding more is mostly mechanical.


def get_greeting(name):
    pieces = ["Hello", name]
    return pieces


name = input("Tell me your name!")
pieces = get_greeting(name)
greeting = " ".join(pieces)
print(greeting)
