"""
Contains all rewrite targets.
"""
import ast


# special wilcard that can be used in a rewrite rule to match all
# rewrite targets
ALL = ".*"


class RewriteTarget:
    """
    Base class.
    """
    def __init__(self, name, mangle=True):
        assert name is not None
        if mangle:
            # some name mangling is required, otherwise the associated rewrite
            # rule will match the node after it has been rewritten, for example
            # elisp: if -> (if
            # the function (if ... should not get rewritten
            self._name = "<>__%s" % name
        else:
            self._name = name

    @property
    def name(self):
        return self._name

    def __str__(self):
        return self.name


class Keyword(RewriteTarget):
    """
    The keywords and special constructs that can be targets of rewrite rules.

    Keywords are defined using all caps so that they do not clash with the
    actual keywords.
    """
    def __init__(self, name):
        super().__init__(name)

    @classmethod
    @property
    def IF(clazz):
        return Keyword("if")

    @classmethod
    @property
    def IF_EXPR(clazz):
        """
        3 if 2 > 1 else 4
        """
        return Keyword("if_expr")

    @classmethod
    @property
    def FOR(clazz):
        return Keyword("for")

    @classmethod
    @property
    def LIST_COMP(clazz):
        return Keyword("list_comp")


class Operator(RewriteTarget):
    """
    The operators that can be targets of rewrite rules.
    """
    def __init__(self, name, assign_to_self_target=None):
        super().__init__(name)
        self._assign_to_self_target = assign_to_self_target

    @property
    def assign_to_self(self):
        """
        + -> +=
        - -> -=
        * -> *=
        etc
        """
        assert self._assign_to_self_target is not None
        return self._assign_to_self_target

    @classmethod
    @property
    def SUBSCRIPT(clazz):
        return Operator("[]")

    @classmethod
    @property
    def AND(clazz):
        return Operator("and")

    @classmethod
    @property
    def OR(clazz):
        return Operator("or")

    @classmethod
    @property
    def ASSIGNMENT(clazz):
        return Operator("=")

    @classmethod
    @property
    def DICT_ASSIGNMENT(clazz):
        """
        Python syntax for associating a value with a dictionary key:
        d["test"] = 1
        """
        return Operator("[]=")

    @classmethod
    @property
    def EQUALS(clazz):
        return Operator("==")

    @classmethod
    @property
    def NOT_EQUALS(clazz):
        return Operator("!=")

    @classmethod
    @property
    def IS_SAME(clazz):
        return Operator("is")

    @classmethod
    @property
    def IS_NOT_SAME(clazz):
        return Operator("is_not")

    @classmethod
    @property
    def LESS_THAN(clazz):
        return Operator("<")

    @classmethod
    @property
    def GREATER_THAN(clazz):
        return Operator(">")

    @classmethod
    @property
    def AUG_ADD(clazz):
        """
        +=, for ex n += 1
        """
        return Operator("+=")

    @classmethod
    @property
    def ADD(clazz):
        return Operator("+", Operator.AUG_ADD)

    @classmethod
    @property
    def U_ADD(clazz):
        """
        +n, for ex +10, +20, +1.3
        """
        return Operator("+n")

    @classmethod
    @property
    def AUG_DIV(clazz):
        """
        /=, for ex n /= 2
        """
        return Operator("/=")

    @classmethod
    @property
    def DIV(clazz):
        return Operator("/", Operator.AUG_DIV)

    @classmethod
    @property
    def AUG_MULT(clazz):
        """
        *=, for ex n *= 1
        """
        return Operator("*=")

    @classmethod
    @property
    def MULT(clazz):
        return Operator("*", Operator.AUG_MULT)

    @classmethod
    @property
    def AUG_SUB(clazz):
        """
        -=, for ex n -= 1
        """
        return Operator("-=")

    @classmethod
    @property
    def SUB(clazz):
        return Operator("-", Operator.AUG_SUB)

    @classmethod
    @property
    def U_SUB(clazz):
        """
        -n, for ex -1, -10, -1.2
        """
        return Operator("-n")

    @classmethod
    @property
    def U_NOT(clazz):
        """
        not b, for ex not True
        """
        return Operator("not")

    @classmethod
    @property
    def MOD(clazz):
        return Operator("%")

    @classmethod
    def forNode(clazz, node):
        if isinstance(node, ast.Add):
            return Operator.ADD
        elif isinstance(node, ast.And):
            return Operator.AND
        elif isinstance(node, ast.Div):
            return Operator.DIV
        elif isinstance(node, ast.Eq):
            return Operator.EQUALS
        elif isinstance(node, ast.Gt):
            return Operator.GREATER_THAN
        elif isinstance(node, ast.Is):
            return Operator.IS_SAME
        elif isinstance(node, ast.IsNot):
            return Operator.IS_NOT_SAME
        elif isinstance(node, ast.Lt):
            return Operator.LESS_THAN
        elif isinstance(node, ast.Mod):
            return Operator.MOD
        elif isinstance(node, ast.Mult):
            return Operator.MULT
        elif isinstance(node, ast.Not):
            return Operator.U_NOT
        elif isinstance(node, ast.NotEq):
            return Operator.NOT_EQUALS
        elif isinstance(node, ast.Or):
            return Operator.OR
        elif isinstance(node, ast.Sub):
            return Operator.SUB
        elif isinstance(node, ast.UAdd):
            return Operator.U_ADD
        elif isinstance(node, ast.USub):
            return Operator.U_SUB
        else:
            raise AssertionError("Unknown node: %s" % node)
