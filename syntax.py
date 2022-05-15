import asttoken
import function
import null


class Argument:
    """
    Describes a function (== method) argument.
    """

    def __init__(self, node, type):
        """
        node: the argument AST node
        type: the type of the argument
        """
        self.node = node
        self.type = type


class Function:
    """
    Describes a function invocation.
    """

    def __init__(self, py_name, py_type, target_name, function_rewrite=None):
        self.py_name = py_name
        self.py_type = py_type
        self.target_name = target_name
        self.function_rewrite = function_rewrite


class TypeMapping:

    def __init__(self, py_type, target_type_name, literal_converter):
        self.py_type = py_type
        self.target_type_name = target_type_name
        self.literal_converter = literal_converter


class TypeMapper:

    def __init__(self):
        self._py_type_to_type_mapping = {}

    def register_type_mapping(self, py_type, target_name, literal_converter=None):
        # this isn't really necessary - just for sanity
        assert py_type in (bool, str, int, float, list), "unsupported py type %s" % py_type
        type_mapping = TypeMapping(py_type, target_name, literal_converter)
        self._py_type_to_type_mapping[py_type] = type_mapping

    def lookup_target_type_name(self, type_info):
        """
        Given a context.TypeInfo instance, returns the type name of the target
        syntax.
        """
        value_type = type_info.value_type
        type_mapping = self._py_type_to_type_mapping[value_type]
        target_type_name = type_mapping.target_type_name
        # formalize this a bit more
        if type_info.value_type is list:
            ct = type_info.get_homogeneous_contained_type()
            if ct is not None:
                if "?" in target_type_name:
                    # poc: List<?>
                    ct_mapping = self._py_type_to_type_mapping[ct]
                    ct_name = ct_mapping.target_type_name
                    target_type_name = target_type_name.replace("?", ct_name)
        return target_type_name

    def convert_to_literal(self, value):
        type_mapping = self._py_type_to_type_mapping.get(type(value), None)
        if type_mapping is not None:
            if type_mapping.literal_converter is not None:
                return type_mapping.literal_converter(value)
        return None


# arguably this should just be part of the syntax because the formatting
# is not just pretty printing: returntrue for ex
class AbstractLanguageFormatter:
    """
    Formatting customizations.
    """
    pass


class CommonInfixFormatter(AbstractLanguageFormatter):

    def delim_suffix(self, token, remaining_tokens):
        if asttoken.next_token_has_type(
                remaining_tokens, asttoken.TARGET_DEREF):
            # no space before '.': "foo".startswith("f"), not "foo" .startswith
            return False
        if token.type.is_target_deref:
            # no space after '.': "foo".startswith("f")
            return False
        if token.type.is_func_call:
            # no space after func name: print("foo", ... - not print( "foo", ...
            return False
        if asttoken.is_boundary_ending_before_value_token(
                remaining_tokens, asttoken.FUNC_CALL_BOUNDARY):
            # no space after last func arg: ...,"foo")
            return False
        if len(remaining_tokens) >= 2 and remaining_tokens[0].type.is_list_literal_boundary and remaining_tokens[1].type.is_list_literal_boundary:
            # special case for empty list: l = [] - not l =[]
            return True
        if asttoken.is_boundary_ending_before_value_token(
                remaining_tokens, asttoken.LIST_LITERAL_BOUNDARY):
            # no space after last list literal arg: [..., "foo"]
            return False
        if asttoken.is_boundary_ending_before_value_token(
                remaining_tokens, asttoken.FUNC_ARG):
            # no space after func arg: 1, 2 - not 1 , 2
            return False
        if token.type.is_func_arg and token.is_end:
            # space after arg sep: 1, 2 - not 1,2
            return True
        if (token.type.is_binop_prec and token.is_end and
            asttoken.next_token_has_value(remaining_tokens)):
            # (1 + 1) * 2, not (1 + 1)* 2
            return True
        if asttoken.is_boundary_ending_before_value_token(
                remaining_tokens, asttoken.BINOP_PREC_BIND):
            # (2 + 3 * 4), not (2 + 3 * 4 )
            return False
        return token.type.has_value


class PythonFormatter(CommonInfixFormatter):

    def delim_suffix(self, token, remaining_tokens):
        if asttoken.is_boundary_starting_before_value_token(
                remaining_tokens, asttoken.BLOCK):
            # we want if <cond>: (no space between <cond> and :
            return False
        return super().delim_suffix(token, remaining_tokens)


class JavaFormatter(CommonInfixFormatter):

    def delim_suffix(self, token, remaining_tokens):
        if asttoken.is_boundary_ending_before_value_token(
                remaining_tokens, asttoken.STMT):
            # we want foo; not foo ;
            return False
        if asttoken.is_boundary_ending_before_value_token(
                remaining_tokens, asttoken.FLOW_CONTROL_TEST):
            # we want if (1 == 1), not if (1 == 1 )
            return False
        if asttoken.is_boundary_starting_before_value_token(
                remaining_tokens, asttoken.BLOCK):
            # we want if (1 == 1) {, not if (1 == 1){
            return True
        if token.type.is_block and token.is_end:
            # we want } else, not }else
            return True
        return super().delim_suffix(token, remaining_tokens)


class ElispFormatter(AbstractLanguageFormatter):

    def delim_suffix(self, token, remaining_tokens):
        if token.type.is_func_call_boundary and token.is_start:
            # no space after '('
            return False
        if asttoken.is_boundary_ending_before_value_token(
                remaining_tokens, asttoken.FUNC_CALL_BOUNDARY):
            # no space if next token is ')'
            return False
        return True



class AbstractLanguageSyntax:
    """
    Stateless metadata that describes a Language Syntax.

    TODO instead of start/end delim, refer to a character family, like parens,
    curlys etc
    """

    def __init__(self, is_prefix, null_type,
                 stmt_start_delim, stmt_end_delim,
                 block_start_delim, block_end_delim,
                 flow_control_test_start_delim, flow_control_test_end_delim,
                 arg_delim,
                 strongly_typed,
                 explicit_rtn,
                 has_block_scope,
                 function_signature_template):
        self.is_prefix = is_prefix
        self.null_type = null_type
        self.stmt_start_delim = stmt_start_delim
        self.stmt_end_delim = stmt_end_delim
        self.block_start_delim = block_start_delim
        self.block_end_delim = block_end_delim
        self.flow_control_test_start_delim = flow_control_test_start_delim
        self.flow_control_test_end_delim = flow_control_test_end_delim
        self.arg_delim = arg_delim
        self.strongly_typed = strongly_typed
        self.explicit_rtn = explicit_rtn
        self.has_block_scope = has_block_scope
        if isinstance(function_signature_template, str):
            function_signature_template = function.FunctionSignatureTemplate(function_signature_template)
        self.function_signature_template = function_signature_template

        self.functions = {} # functions_calls_to_rewrite
        self.type_mapper = TypeMapper()

    def to_literal(self, value):
        if value is null.value:
            return self.null_type
        if isinstance(value, str):
            return '"%s"' % str(value)
        literal = self.type_mapper.convert_to_literal(value)
        if literal is None:
            return value
        else:
            return literal

    def to_identifier(self, value):
        return str(value)

    def combine_types(self, lhs, rhs):
        # this is incomplete at best
        # if specialized behavior is necessary, it can move into the
        # TypeMapping class?
        if lhs is float or rhs is float:
            return float
        if lhs is str or rhs is str:
            return str
        return int

    def register_function_rename(self, py_name, py_type, target_name):
        assert not py_name in self.functions
        self.functions[py_name] = Function(py_name, py_type, target_name)

    def register_function_rewrite(self, py_name, py_type, rewrite, target_name=None):
        assert not py_name in self.functions
        function = Function(py_name, py_type, target_name=target_name, function_rewrite=rewrite)
        self.functions[py_name] = function


class PythonSyntax(AbstractLanguageSyntax):

    def __init__(self):
        """
        : is the block start delim
        """
        super().__init__(is_prefix=False, null_type="None",
                         stmt_start_delim="", stmt_end_delim="",
                         block_start_delim=":", block_end_delim="",
                         flow_control_test_start_delim="", flow_control_test_end_delim="",
                         arg_delim=",",
                         strongly_typed=False,
                         explicit_rtn=True,
                         has_block_scope=False,
                         function_signature_template="def $func_name($args_start$arg_name, $args_end)")


class JavaSyntax(AbstractLanguageSyntax):

    def __init__(self):
        super().__init__(is_prefix=False, null_type="null",
                         stmt_start_delim="", stmt_end_delim=";",
                         block_start_delim="{", block_end_delim="}",
                         flow_control_test_start_delim="(", flow_control_test_end_delim=")",
                         arg_delim=",",
                         strongly_typed=True,
                         explicit_rtn=True,
                         has_block_scope=True,
                         function_signature_template="$visibility $rtn_type $func_name($args_start$arg_type $arg_name, $args_end)")

        self.type_mapper.register_type_mapping(int,  "int")
        self.type_mapper.register_type_mapping(float,  "float")
        self.type_mapper.register_type_mapping(str,  "String")
        self.type_mapper.register_type_mapping(bool, "boolean", lambda v: "true" if v else "false")
        self.type_mapper.register_type_mapping(list, "List<?>")

        print_fmt = {int: "%d", float: "%d", str: "%s"}
        self.register_function_rewrite(
            py_name="print", py_type=None,
            target_name="System.out.println",
            rewrite=lambda args, rw:
                rw.replace_args_with(
                  rw.call("String.format")
                    .prepend_arg(" ".join([print_fmt[a.type] for a in args]))
                      .append_args([a.node for a in args]))
                if len(args) > 1 else None)

        self.register_function_rewrite(
            py_name="len", py_type=None,
            rewrite=lambda args, rw:
                rw.rewrite_as_attr_method_call().rename("length"))

        self.register_function_rewrite(
            py_name="<>_==", py_type=None,
            rewrite=lambda args, rw:
                rw.replace_node_with(rw.call("equals"))
                  .rewrite_as_attr_method_call() # equals(s s2) -> s.equals(s2)
                if args[0].type == str else None) # only for str...for now FIX

        self.register_function_rename(py_name="endswith", py_type=str,
                                      target_name="endsWith")
        self.register_function_rename(py_name="startswith", py_type=str,
                                      target_name="startsWith")

        self.register_function_rewrite(py_name="<>_new_list", py_type=list,
            rewrite=lambda args, rw:
                # this re-writes the ast.List node as a call node
                # todo import java.util.List
                rw.replace_node_with(rw.call("List.of")))
        self.register_function_rename(py_name="append", py_type=list,
                                      target_name="add")


class ElispSyntax(AbstractLanguageSyntax):

    def __init__(self):
        super().__init__(is_prefix=True, null_type="nil",
                         stmt_start_delim="", stmt_end_delim="",
                         block_start_delim="", block_end_delim="",
                         flow_control_test_start_delim="", flow_control_test_end_delim="",
                         arg_delim=" ",
                         strongly_typed=False,
                         explicit_rtn=False,
                         has_block_scope=False,
                         function_signature_template="(defun2 $func_name ($args_start$arg_name $args_end)")

        self.type_mapper.register_type_mapping(bool, None, lambda v: "t" if v else "nil")

        self.register_function_rewrite(py_name="<>_new_list", py_type=list,
            rewrite=lambda args, rw:
                # this re-writes the ast.List node as a call node
                rw.replace_node_with(rw.call("list")))

        self.register_function_rewrite(
            py_name="append", py_type=list,
            target_name="append",
            rewrite=lambda args, rw: rw
                .rewrite_as_func_call(inst_1st=True)
                .reassign_to_arg())

        self.register_function_rewrite(
            py_name="<>_=", py_type=None,
            rewrite=lambda args, rw: rw.replace_node_with(rw.call("setq").stmt()))
        self.register_function_rewrite(
            py_name="print", py_type=None,
            target_name="message",
            rewrite=lambda args, rw:
                rw.prepend_arg(" ".join(["%s" for a in args]))
                if len(args) > 1 or (len(args) == 1 and args[0].type != str) else None)
        self.register_function_rewrite(
            py_name="<>_+", py_type=None,
            rewrite=lambda args, rw:
                rw.replace_node_with(rw.call("concat")
                    .append_args(
                        [a.node if a.type == str else rw.call("int-to-string")
                            .append_arg(a.node) for a in args]),
                keep_args=False)
                if args[0].type == str else
                # this re-writes the ast.BinOp node as a call node
                rw.replace_node_with(rw.call("+")))

        self.register_function_rewrite(
            py_name="<>_*", py_type=None,
            rewrite=lambda args, rw:
                # this re-writes the ast.Binop node as a call node
                rw.replace_node_with(rw.call("*")))

        def _defun_rewrite(args, rw):
            f = rw.call("defun").stmt() # stmt so (defun ..) is followed by \n
            f.prepend_arg(rw.ident(rw.node.name))
            if len(args) == 0:
                args_list = rw.call("") # does this work for no args?
            else:
                args_list = rw.call(args[0].node.arg)
                for i, arg in enumerate(args):
                    if i > 0:
                        args_list.append_arg(rw.ident(arg.node.arg))
            f.append_arg(args_list)

            rw.wrap(rw.node.body[0]).newline().indent_incr()
            rw.wrap(rw.node.body[-1]).newline().indent_decr()
            f.append_args(rw.node.body)
            rw.replace_node_with(f, keep_args=False)
        self.register_function_rewrite(py_name="<>_funcdef", py_type=None, rewrite=_defun_rewrite)

        def _if_rewrite(args, rw):
            if_func = rw.call("if").stmt() # stmt so (if ..) is followed by \n
            assert len(rw.node.body) >= 1
            if_block_has_single_stmt = len(rw.node.body) == 1
            else_block_exists = len(rw.node.orelse) > 0
            if if_block_has_single_stmt:
                body = rw.wrap(rw.node.body[0]).newline().indent_incr()
                if not else_block_exists:
                    body.indent_decr()
                if_func.append_arg(body)
            else:
                progn = rw.call("progn").newline().indent_around()
                rw.wrap(rw.node.body[0]).newline().indent_incr()
                rw.wrap(rw.node.body[-1]).newline().indent_decr()
                progn.append_args(rw.node.body)
                if_func.append_arg(progn)
            if else_block_exists:
                if_func.append_args([rw.wrap(n) for n in rw.node.orelse])
                if not if_block_has_single_stmt:
                    rw.wrap(rw.node.orelse[0]).newline().indent_incr()
                rw.wrap(rw.node.orelse[-1]).newline().indent_decr()
            rw.replace_node_with(if_func)
        self.register_function_rewrite(py_name="<>_if", py_type=None, rewrite=_if_rewrite)

        self.register_function_rewrite(
            py_name="<>_==", py_type=None,
            rewrite=lambda args, rw:
                rw.replace_node_with(rw.call("equal")))

        self.register_function_rewrite(
            py_name="endswith", py_type=str,
            target_name="string-suffix-p",
            rewrite=lambda args, rw: rw.rewrite_as_func_call())

        self.register_function_rewrite(
            py_name="startswith", py_type=str,
            target_name="string-prefix-p",
            rewrite=lambda args, rw: rw.rewrite_as_func_call())

        self.register_function_rename(py_name="len", py_type=None, target_name="length")
