import ast as astm
import os
import syntax


class BinOp:

    def __init__(self, op, precedence):
        self.op = op
        self.precedence = precedence

    def __str__(self):
        return self.op


ADD_BINOP = BinOp("+", 1)
MULT_BINOP = BinOp("*", 2)


class NoopNodeVisitor:

    def add(self, node, num_children_visited):
        pass

    def binop(self, node, num_children_visited):
        pass

    def assign(self, node, num_children_visited):
        pass

    def call(self, node, num_children_visited):
        pass

    def compare(self, node, num_children_visited):
        pass

    def cond_if(self, node, num_children_visited):
        pass

    def cond_else(self, node, num_children_visited):
        pass

    def constant(self, node, num_children_visited):
        pass

    def eq(self, node, num_children_visited):
        pass

    def expr(self, node, num_children_visited):
        pass

    def module(self, node, num_children_visited):
        pass

    def name(self, node, num_children_visited):
        pass

    def name_constant(self, node, num_children_visited):
        pass

    def num(self, node, num_children_visited):
        pass

    def rtn(self, node, num_children_visited):
        pass

    def string(self, node, num_children_visited):
        pass


class AbstractLanguageEmitterVisitor(NoopNodeVisitor):

    def __init__(self, language_syntax):
        self.language_syntax = language_syntax
        self.tokens = []
        self.indentation_level = 0
        self.binop_stack = []

    def expr(self, node, num_children_visited):
        if num_children_visited == -1:
            self.end_statement()

    def start_block(self):
        self.indentation_level += 1

    def end_block(self):
        self.indentation_level -= 1

    def end_statement(self):
        self.append(self.language_syntax.statement_delim)
        self.append("\n")

    def binop_start(self, binop):
        self.binop_stack.append(binop)

    def binop_end(self, binop):
        self.binop_stack.pop()

    def binop_arg_requires_parens(self):
        if len(self.binop_stack) > 1:
            current_op = self.binop_stack[-1]
            parent_op = self.binop_stack[-2]
            if current_op.precedence < parent_op.precedence:
                return True
        return False

    def get_indentation(self):
        return "    "*self.indentation_level
    
    def append(self, token):
        if len(self.tokens) > 0 and self.tokens[-1] == "\n":
            self.tokens.append(self.get_indentation())
        self.tokens.append(str(token))
        if self.language_syntax.token_requires_sep(token):
            self.tokens.append(" ")

    def __str__(self):
        return "".join(self.tokens).strip()


class LanguageEmitterVisitor(AbstractLanguageEmitterVisitor):
    """
    The class hierarchy doesn't make sense, really.  Why inherit from
    AbstractLanguageEmitterVisitor?
    """

    def __init__(self, language_syntax):
        super().__init__(language_syntax)

    # BINOP START
    
    def binop(self, node, num_children_visited):
        if isinstance(node.op, astm.Add):
            binop = ADD_BINOP
        elif isinstance(node.op, astm.Mult):
            binop = MULT_BINOP
        else:
            assert False, "bad binop"

        if num_children_visited == 0:
            self.binop_start(binop)
            if self.binop_arg_requires_parens():
                self.append("(")
        elif num_children_visited == -1:
            # visited: left, op, right
            if self.binop_arg_requires_parens():
                self.append(")")
            self.binop_end(binop)

    def add(self, node, num_children_visited):
        self.append("+")

    def mult(self, node, num_children_visited):
        self.append("*")

    # BINOP END

    def assign(self, node, num_children_visited):
        if num_children_visited == 1:
            self.append("=")
        elif num_children_visited == -1:
            self.end_statement()        

    def call(self, node, num_children_visited):
        if num_children_visited == 0:
            py_func_name = node.func.id
            func = self.language_syntax.functions.get(py_func_name, None)
            func_name = py_func_name if func is None else func.target_name
            self.append(func_name)
            self.append("(")
        elif num_children_visited == -1:
            self.append(")")

    def cond_if(self, node, num_children_visited):
        if num_children_visited == 0:
            self.append("if")
        elif num_children_visited == 1:
            self.append(self.language_syntax.block_start_delim)
            self.end_statement()
            self.start_block()
        elif num_children_visited == -1:
            self.end_block()

    def cond_else(self, node, num_children_visited):
        if num_children_visited == 0:
            self.append("else")
            self.append(self.language_syntax.block_start_delim)
            self.end_statement()
            self.start_block()
        elif num_children_visited == -1:
            self.end_block()
            
    def constant(self, node, num_children_visited):
        self.append(self.language_syntax.to_literal(node.value))

    def eq(self, node, num_children_visited):
        self.append("==")

    def name(self, node, num_children_visited):
        self.append(self.language_syntax.to_identifier(node.id))

    def name_constant(self, node, num_children_visited):
        self.append(self.language_syntax.to_identifier(node.value))        

    def num(self, node, num_children_visited):
        self.append(self.language_syntax.to_literal(node.n))

    def rtn(self, node, num_children_visited):
        if num_children_visited == 0:
            self.append("return")
        elif num_children_visited == -1:
            self.end_statement()        

    def string(self, node, num_children_visited):
        self.append(self.language_syntax.to_literal(node.s))


def run(code, language_syntax):
    ast = astm.parse(code)
    visitor = LanguageEmitterVisitor(language_syntax)
    _run(ast, visitor)
    return str(visitor)


def _run(node, visitor):
    # BINOP
    if isinstance(node, astm.Add):
        visitor.add(node, 0)
    elif isinstance(node, astm.Mult):
        visitor.mult(node, 0)
    elif isinstance(node, astm.BinOp):
        visitor.binop(node, 0)
        _run(node.left, visitor)
        visitor.binop(node, 1)
        _run(node.op, visitor)
        visitor.binop(node, 2)
        _run(node.right, visitor)
        visitor.binop(node, -1)
    # OTHER
    elif isinstance(node, astm.Assign):
        assert len(node.targets) == 1
        visitor.assign(node, 0)
        _run(node.targets[0], visitor)
        visitor.assign(node, 1)
        _run(node.value, visitor)
        visitor.assign(node, -1)
    elif isinstance(node, astm.Call):
        visitor.call(node, 0)
        for i, arg in enumerate(node.args):
            _run(arg, visitor)
            visitor.call(node, i+1)
        for keyword in node.keywords:
            assert False, "keywords not handled"
        visitor.call(node, -1) # last
    elif isinstance(node, astm.Constant):
        visitor.constant(node, 0)
    elif isinstance(node, astm.Compare):
        visitor.compare(node, 0)
        _run(node.left, visitor)
        assert len(node.ops) == 1
        visitor.compare(node, 1)
        _run(node.ops[0], visitor)
        assert len(node.comparators) == 1
        visitor.compare(node, 2)
        _run(node.comparators[0], visitor)
        visitor.compare(node, -1)
    elif isinstance(node, astm.Eq):
        visitor.eq(node, 0)        
    elif isinstance(node, astm.If):
        visitor.cond_if(node, 0)
        _run(node.test, visitor)
        visitor.cond_if(node, 1)
        for i, b in enumerate(node.body):
            _run(b, visitor)
            visitor.cond_if(node, i+2)
        visitor.cond_if(node, -1)
        if len(node.orelse) > 0:
            visitor.cond_else(node, 0)
            for i, b in enumerate(node.orelse):
                _run(b, visitor)
                visitor.cond_else(node, i+1)
            visitor.cond_else(node, -1)
    elif isinstance(node, astm.Module):
        visitor.module(node, 0)
        for i, body in enumerate(node.body):
            _run(body, visitor)
            visitor.module(node, i+1)
    elif isinstance(node, astm.Name):
        visitor.name(node, 0)
    elif isinstance(node, astm.NameConstant):
        visitor.name_constant(node, 0)
    elif isinstance(node, astm.Num):
        visitor.num(node, 0)
    elif isinstance(node, astm.Expr):
        visitor.expr(node, 0)
        _run(node.value, visitor)
        visitor.expr(node, -1)
    elif isinstance(node, astm.Return):
        visitor.rtn(node, 0)
        _run(node.value, visitor)
        visitor.rtn(node, -1)        
    elif isinstance(node, astm.Str):
        visitor.string(node, 0)        
    else:
        assert False, "Unknown node %s" % node


if __name__ == "__main__":
    #syntax = PythonSyntax()
    syntax = syntax.JavaSyntax()
    with open("test.py", "r") as f:
        print(run(f.read(), syntax))
