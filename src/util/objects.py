def assert_isinstance(thing, expected_types):
    assert instanceof(thing, expected_types), "Bad type [%s], expected %s" % (thing, expected_types)


def instanceof(thing, the_type):
    if isinstance(thing, (StrictReadOnly, SilentReadOnly)):
        thing = thing._o
    return isinstance(thing, the_type)


class StrictReadOnly:
    """
    Errors out of any attributes are set.
    
    Usage of this wrapper breaks isinstance, call objects.instanceof instead.
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

    def __deepcopy__(self, d):
        # ro, so no need to copy
        return self


class SilentReadOnly:
    """
    Ignores attempts to set attributes.

    Usage of this wrapper breaks isinstance, call objects.instanceof instead.
    """

    def __init__(self, o):
        self._o = o

    def __getattr__(self, name):
        return getattr(self._o, name)

    def __setattr__(self, name, value):
        if name == "_o":
            object.__setattr__(self, name, value)

    def __deepcopy__(self, d):
        # ro, so no need to copy
        return self
