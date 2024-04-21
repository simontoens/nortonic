import os

# add tests for this to tests/test_golang.
def greet(name):
    if name is None:
        print("Hello, stranger!")
    else:
        print("hello", name)

def main2(name):
    greet(name)
    greet(None)

greet("Bernd")
main2("Simon")

