import ast


class CallAsKeyword(ast.Call):
    """
    A marker class that indicates that this call node should be treated
    like a keyword.

    For example:
    print "foo" "blah" instead of print("foo", "blah")
    range l instead of range(l)

    The difference is really just syntactic sugar.
    """
    pass
