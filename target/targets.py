def is_python(target):
    """
    Hack to see if we are translating to Python.
    """
    return "python" in str(type(target))

