class StrictReadOnly:
    """
    Errors out of any attributes are set. This breaks isinstance.
    """

    def __init__(self, o):
        self._o = o

    def __getattr__(self, name):
        return getattr(self._o, name)

    def __setattr__(self, name, value):
        if name == "_o":
            object.__setattr__(self, name, value)
        else:
            raise Exception("cannot set %s on ro instance" % name)


class SneakyReadOnly:
    """
    Ignores attempts to set attributes. This breaks isinstance.
    """

    def __init__(self, o):
        self._o = o

    def __getattr__(self, name):
        return getattr(self._o, name)

    def __setattr__(self, name, value):
        if name == "_o":
            object.__setattr__(self, name, value)
