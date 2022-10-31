class ScopeEnum:

    @property
    def function(self):
        return self == FUNCTION

    @property
    def module(self):
        return self == MODULE

    def __str__(self):
        if self.function:
            return "Scope: Function"
        else:
            return "Scope: Module"


FUNCTION = ScopeEnum()
MODULE = ScopeEnum()
