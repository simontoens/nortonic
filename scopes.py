class ScopeEnum:

    @property
    def loop_for(self):
        return self is LOOP_FOR

    @property
    def function(self):
        return self is FUNCTION

    @property
    def module(self):
        return self is MODULE

    @property
    def block(self):
        return self is LOOP_FOR or self is FUNCTION

    def __str__(self):
        if self.function:
            return "Scope: Function"
        else:
            return "Scope: Module"


FUNCTION = ScopeEnum()
MODULE = ScopeEnum()
LOOP_FOR = ScopeEnum()
