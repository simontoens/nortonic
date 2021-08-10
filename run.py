import ast as astm
import os


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
            self.append(node.func.id)
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


class AbstractLanguageSyntax:
    """
    Stateless metadata that describes a Language Syntax.
    """
    
    def __init__(self, is_prefix, statement_delim,
                 block_start_delim, block_end_delim,
                 block_indentation,
                 tokens_requiring_sep):
        self.is_prefix = is_prefix
        self.statement_delim = statement_delim
        self.block_start_delim = block_start_delim
        self.block_end_delim = block_end_delim
        self.block_indentation = block_indentation
        self.tokens_requiring_sep = tokens_requiring_sep

    def to_literal(self, value):
        if isinstance(value, str):
            return '"%s"' % str(value)
        return value

    def to_identifier(self, value):
        return str(value)

    def token_requires_sep(self, token):
        return token in self.tokens_requiring_sep
                      

class PythonSyntax(AbstractLanguageSyntax):
    
    def __init__(self):
        super().__init__(is_prefix=False,
                         statement_delim="",
                         block_start_delim=":",
                         block_end_delim="",
                         block_indentation="  ",
                         tokens_requiring_sep=("if", "return",))


class AbstractEmitter:

    def __init__(self):
        self.tokens = []
        self.op_stack = []
        self.indentation_level = 0
        self.requires_indentation = False

    def append(self, token, requires_space_sep=False):
        indentation = " "*self.indentation_level if self.requires_indentation else ""
        self.tokens.append("%s%s" % (indentation, str(token)))
        if requires_space_sep:
            self.tokens.append(" ")
        self.requires_indentation = False

    def ass(self):
        self.append("=")

    def identifier(self, v):
        self.append(v)

    def constantValue(self, v):
        if isinstance(v, str):
            self.string(v)
        elif isinstance(v, int):
            self.number(v)            
        else:
            assert False, "constant type not handled: %s" % v.__class__

    def block_start(self):
        self.indentation_level += 2

    def block_end(self):
        self.indentation_level -= 2

    def if_start(self):
        self.stmt_start()
        self.append("if", requires_space_sep=True)

    def if_end(self):
        self.append(":\n")

    def else_start(self):
        self.stmt_start()
        self.append("else:\n")

    def else_end(self):
        pass

    def number(self, n):
        self.append(n)

    def string(self, s):
        self.append('"%s"' % s)

    def functionCallStart(self, function_name):
        self.append(function_name)
        self.append("(")

    def functionCallEnd(self, function_name):
        self.append(")")

    def binop_start(self, op):
        self.op_stack.append(op)

    def binop_end(self, op):
        assert(op == self.op_stack.pop())

    def binop_leftarg_start(self):
        if self._binop_require_parens():
            self.append("(")

    def binop_leftarg_end(self):
        self.append(self.op_stack[-1])

    def binop_rightarg_start(self):
        pass

    def binop_rightarg_end(self):
        if self._binop_require_parens():
            self.append(")")

    def eq(self):
        self.append("==")

    def rtn(self):
        self.append("return", requires_space_sep=True)

    def _binop_require_parens(self):
        if len(self.op_stack) > 1:
            current_op = self.op_stack[-1]
            parent_op = self.op_stack[-2]
            if current_op.precedence < parent_op.precedence:
                return True
        return False

    def stmt_start(self):
        self.requires_indentation = True

    def stmt_end(self):
        self.tokens.append("\n")

    def try_stmt(self):
        self.stmt_start()
        self.append("try:")
        self.stmt_end()

    def except_stmt(self):
        self.stmt_start()
        self.append("except:")
        self.stmt_end()

    def __str__(self):
        return "".join([str(t) for t in self.tokens]).strip()


class PythonEmitter(AbstractEmitter):
    pass


def visit_assign_node(node, emitter):
    assert len(node.targets) == 1
    process_node(node.targets[0], emitter)
    emitter.ass()
    process_node(node.value, emitter)


def visit_binop_node(node, emitter):
    op = process_node(node.op, emitter)
    emitter.binop_start(op)
    emitter.binop_leftarg_start()
    process_node(node.left, emitter)
    emitter.binop_leftarg_end()
    emitter.binop_rightarg_start()
    process_node(node.right, emitter)
    emitter.binop_rightarg_end()
    emitter.binop_end(op)


def visit_expr_node(node, emitter):
    process_node(node.value, emitter)
    

def visit_call_node(node, emitter):
    fname = node.func.id
    emitter.functionCallStart(fname)
    for arg in node.args:
        process_node(arg, emitter)
    for keyword in node.keywords:
        assert False, "keywords not handled"
    emitter.functionCallEnd(fname)


def visit_compare_node(node, emitter):
    process_node(node.left, emitter)
    assert len(node.ops) == 1
    process_node(node.ops[0], emitter)
    assert len(node.comparators) == 1
    process_node(node.comparators[0], emitter)


def visit_if_node(node, emitter):
    emitter.if_start()
    process_node(node.test, emitter)
    emitter.if_end()
    process_body(node.body, emitter)
    if len(node.orelse) > 0:
        emitter.else_start()
        process_body(node.orelse, emitter)
        emitter.else_end()


def visit_add_node(node, emitter):
    return ADD_OP


def visit_mod_node(node, emitter):
    return MOD_OP


def visit_mult_node(node, emitter):
    return MULT_OP


def visit_eq_node(node, emitter):
    emitter.eq()


def visit_module_node(node, emitter):
    # is_block is False because module level
    process_body(node.body, emitter, is_block=False)

def process_body(body, emitter, is_block=True):
    if is_block:
        emitter.block_start()
    for stmt in body:
        emitter.stmt_start()
        process_node(stmt, emitter)
        emitter.stmt_end()
    if is_block:
        emitter.block_end()


def visit_name_node(node, emitter):
    name = node.id
    emitter.identifier(name)
    return name


def visit_constant_node(node, emitter):
    val = node.value
    emitter.constantValue(val)
    return val


def visit_return_node(node, emitter):
    emitter.rtn()
    process_node(node.value, emitter)


def visit_try_node(node, emitter):
    emitter.try_stmt()
    process_body(node.body, emitter)
    for handler in node.handlers:
        process_node(handler, emitter)
    assert len(node.orelse) == 0


def visit_unknown_node(node, emitter):
    print("This unknown node is a", node)
    for field_name in node._fields:
        print("Looking at field", field_name)
        field_value = getattr(node, field_name)
        if isinstance(field_value, (list, tuple)):
            for child in field_value:
                process_node(child, emitter)
        else:
            if isinstance(field_value, astm.AST):
                process_node(field_value, emitter)
            else:
                print("Value:", field_value)


visit_functions = {
    astm.Add: visit_add_node,
    astm.Assign: visit_assign_node,
    astm.BinOp: visit_binop_node,
    astm.Call: visit_call_node,
    astm.Compare: visit_compare_node,
    astm.Constant: visit_constant_node,
    astm.Expr: visit_expr_node,
    astm.Eq: visit_eq_node,
    astm.If: visit_if_node,
    astm.Mod: visit_mod_node,
    astm.Module: visit_module_node,
    astm.Mult: visit_mult_node,
    astm.Name: visit_name_node,
    astm.Return: visit_return_node,
    astm.Try: visit_try_node,
}


def process_node(node, emitter):
    visitf = visit_unknown_node
    visitf = visit_functions.get(node.__class__, visitf)
    return visitf(node, emitter)


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
    syntax = PythonSyntax()
    with open("test.py", "r") as f:
        print(run(f.read(), syntax))
