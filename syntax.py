import ast
import asttoken
import context
import function
import functools
import nodebuilder


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


class SimpleTypeMapping:

    def __init__(self, py_type, target_type_name, literal_converter):
        self.py_type = py_type
        self.target_type_name = target_type_name
        self.literal_converter = literal_converter
        self.is_container_type = False


class ContainerTypeMapping:

    def __init__(self, py_type, target_type_name, start_literal, end_literal, value_separator):
        self.py_type = py_type
        self.target_type_name = target_type_name
        self.start_literal = start_literal
        self.end_literal = end_literal
        self.value_separator = value_separator
        self.is_container_type = True


class TypeMapper:

    def __init__(self):
        self._py_type_to_type_mapping = {}

    def register_none_type_name(self, target_name):
        self.register_simple_type_mapping(type(None), target_name, lambda v: target_name)

    def register_simple_type_mapping(self, py_type, target_name, literal_converter=None):
        if isinstance(py_type, context.TypeInfo):
            # if the python type requires in import, it is easier to pass it in
            # as a TypeInfo constant
            py_type = py_type.value_type
        m = SimpleTypeMapping(py_type, target_name, literal_converter)
        self._py_type_to_type_mapping[py_type] = m

    def register_container_type_mapping(self, py_type, target_name, start_literal, end_literal, values_separator=None):
        m = ContainerTypeMapping(py_type, target_name, start_literal,
                                 end_literal, values_separator)
        self._py_type_to_type_mapping[py_type] = m

    def lookup_target_type_name(self, type_info):
        """
        Given a context.TypeInfo instance, returns the type name of the target
        syntax, as a string.
        """
        py_type = self._get_py_type(type_info)
        if py_type is None.__class__:
            return None
        assert py_type in self._py_type_to_type_mapping, "Missing type mapping for %s" % py_type
        type_mapping = self._py_type_to_type_mapping[py_type]
        target_type_name = type_mapping.target_type_name
        if type_mapping.is_container_type:
            if "<?>" in target_type_name:
                # the presence of this magic string indicates that the
                # target type can be assigned a contained type
                # this probably should get fixed up a bit
                contained_type_info = type_info.get_contained_type_info()
                if contained_type_info is not None:                
                    contained_types = contained_type_info.get_value_types()
                    contained_type_names = [self._py_type_to_type_mapping[t].target_type_name for t in contained_types]
                    target_type_name = target_type_name.replace("<?>", "<%s>" % ", ".join(contained_type_names))
        return target_type_name


    def get_type_mapping(self, type_info):
        """
        Given a context.TypeInfo instance, returns the TypeMapping instance.
        syntax, as a string.
        """
        py_type = self._get_py_type(type_info)
        return self._py_type_to_type_mapping[py_type]

    def convert_to_literal(self, value):
        value_type = value if isinstance(value, type) else type(value)
        type_mapping = self._py_type_to_type_mapping.get(value_type, None)
        if type_mapping is not None:
            assert not type_mapping.is_container_type
            if type_mapping.literal_converter is not None:
                return type_mapping.literal_converter(value)
        return None

    def _get_py_type(self, type_info):
        return type_info.value_type


# arguably this should just be part of the syntax because the formatting
# is not just pretty printing: returntrue for ex
class AbstractLanguageFormatter:
    """
    Formatting customizations.
    """
    def delim_suffix(self, token, remaining_tokens):
        if token.type.is_unaryop:
            # i = -1, not i = - 1
            return False
        return token.type.has_value


class CommonInfixFormatter(AbstractLanguageFormatter):

    def delim_suffix(self, token, remaining_tokens):
        if asttoken.next_token_has_type(remaining_tokens, asttoken.TARGET_DEREF):
            # no space before '.': "foo".startswith("f"), not "foo" .startswith
            return False
        if token.type.is_target_deref:
            # no space after '.': "foo".startswith("f")
            return False
        if token.type.is_func_call_boundary and token.is_end and asttoken.next_token_has_value(remaining_tokens):
            # "foo".length() == 3, not "foo".length()== 3;
            return True
        if asttoken.next_token_has_type(remaining_tokens, asttoken.FUNC_CALL_BOUNDARY) and remaining_tokens[0].is_start:
            # "foo".endswith("blah"), not "foo".endswith ("blah")
            return False
        if asttoken.is_boundary_ending_before_value_token(remaining_tokens, asttoken.FUNC_CALL_BOUNDARY):
            # no space after last func arg: ...,"foo")
            return False
        if asttoken.next_token_has_type(remaining_tokens, asttoken.SUBSCRIPT):
            # no space before subscript start: l[0] not l [0]
            # no space before subscript end: l[0] not l[0 ]
            return False
        if token.type.is_subscript and token.is_end:
            # d["k2"] = 3, not d["k2"]= 3
            return True
        if token.type.is_container_literal_boundary:
            # no space before first container literal arg, for example for list:
            # ["a", ... instead of [ "a", ...
            return False
        if asttoken.is_boundary_ending_before_value_token(remaining_tokens, asttoken.CONTAINER_LITERAL_BOUNDARY):
            # no space after last container literal arg, for example for list:
            # [..., "foo"] instead of [..., "foo" ]
            return False
        if asttoken.next_token_has_type(remaining_tokens, asttoken.VALUE_SEPARATOR):
            # {"key": "value"}, not {"key" : "value"}
            return False
        if token.type.is_value_sep and asttoken.next_next_token_has_type(remaining_tokens, asttoken.SUBSCRIPT, is_end=True):
            # "foo"[1:2], not "foo"[1: 2]
            return False
        if token.type.is_value_sep and asttoken.next_token_has_type(remaining_tokens, asttoken.UNARYOP) and asttoken.next_next_token_has_type(remaining_tokens[1:], asttoken.SUBSCRIPT, is_end=True):
            # "foo"[1:-2], not "foo"[1: -2]
            return False
        if asttoken.is_boundary_ending_before_value_token(remaining_tokens, asttoken.FUNC_ARG):
            # no space after func arg: 1, 2 - not 1 , 2
            return False
        if token.type.is_func_arg and token.is_end:
            # space after arg sep: 1, 2 - not 1,2
            return True
        if (token.type.is_binop_prec and token.is_end and asttoken.next_token_has_value(remaining_tokens)):
            # (1 + 1) * 2, not (1 + 1)* 2
            return True
        if asttoken.is_boundary_ending_before_value_token(remaining_tokens, asttoken.BINOP_PREC_BIND):
            # (2 + 3 * 4), not (2 + 3 * 4 )
            return False
        return super().delim_suffix(token, remaining_tokens)        


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
        if token.type.is_container_literal_boundary and token.is_start:
            if token.value == "(list":
                # for (list 1) we want a space after (list
                if len(remaining_tokens) > 0 and not remaining_tokens[0].type.is_container_literal_boundary:
                    return True
            else:
                return False
        if asttoken.is_boundary_ending_before_value_token(
                remaining_tokens, asttoken.FUNC_CALL_BOUNDARY):
            # no space if next token is ')'
            return False
        if asttoken.is_boundary_ending_before_value_token(
                remaining_tokens, asttoken.CONTAINER_LITERAL_BOUNDARY):
            # no space if next token is ')'
            return False
        return super().delim_suffix(token, remaining_tokens)        


class AbstractLanguageSyntax:
    """
    Stateless metadata that describes a Language Syntax.

    TODO instead of start/end delim, refer to a character family, like parens,
    curlys etc
    """

    def __init__(self, is_prefix=None,
                 stmt_start_delim=None, stmt_end_delim=None,
                 block_start_delim=None, block_end_delim=None,
                 flow_control_test_start_delim=None,
                 flow_control_test_end_delim=None,
                 loop_foreach_keyword=None,
                 arg_delim=None,
                 strongly_typed=None,
                 explicit_rtn=None,
                 has_block_scope=None,
                 ternary_replaces_if_expr = None,
                 function_signature_template=None):
        self.is_prefix = is_prefix
        self.stmt_start_delim = stmt_start_delim
        self.stmt_end_delim = stmt_end_delim
        self.block_start_delim = block_start_delim
        self.block_end_delim = block_end_delim
        self.flow_control_test_start_delim = flow_control_test_start_delim
        self.flow_control_test_end_delim = flow_control_test_end_delim
        self.loop_foreach_keyword = loop_foreach_keyword
        self.arg_delim = arg_delim
        self.strongly_typed = strongly_typed
        self.explicit_rtn = explicit_rtn
        self.has_block_scope = has_block_scope
        self.ternary_replaces_if_expr = ternary_replaces_if_expr
        if isinstance(function_signature_template, str):
            function_signature_template = function.FunctionSignatureTemplate(function_signature_template)
        self.function_signature_template = function_signature_template

        self.functions = {} # functions_calls_to_rewrite
        self.type_mapper = TypeMapper()

    def to_literal(self, value):
        value_type = value if isinstance(value, type) else type(value)
        if value_type is str:
            return '"%s"' % str(value)
        literal = self.type_mapper.convert_to_literal(value)
        return value if literal is None else literal

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

    def get_function_lookup_key(self, func_name, target_type):
        return "%s_%s" % (func_name, str(target_type))
    
    def register_function_rename(self, py_name, py_type, target_name):
        """
        Registers a function rename, for example endswith -> endsWith.
        """
        self.register_function_rewrite(py_name, py_type, rewrite=None, target_name=target_name)

    def register_function_rewrite(self, py_name, py_type, rewrite, target_name=None):
        """
        Registers a function rewrite.

        target_name may be set if the function has to be only renamed (but
        perhaps the function arguments have to be re-written),
        """
        if isinstance(py_type, context.TypeInfo):
            # if the python type requires in import, it is easier to pass it in
            # as a TypeInfo constant
            py_type = py_type.value_type
        key = self.get_function_lookup_key(py_name, py_type)
        assert not key in self.functions
        function = Function(py_name, py_type, target_name=target_name, function_rewrite=rewrite)
        self.functions[key] = function


class PythonSyntax(AbstractLanguageSyntax):

    def __init__(self):
        """
        : is the block start delim
        """
        super().__init__(is_prefix=False,
                         stmt_start_delim="", stmt_end_delim="",
                         block_start_delim=":", block_end_delim="",
                         flow_control_test_start_delim="", flow_control_test_end_delim="",
                         loop_foreach_keyword="in",
                         arg_delim=",",
                         strongly_typed=False,
                         explicit_rtn=True,
                         has_block_scope=False,
                         function_signature_template="def $func_name($args_start$arg_name, $args_end)")

        self.type_mapper.register_none_type_name("None")
        self.type_mapper.register_container_type_mapping(list, "list", "[", "]")
        self.type_mapper.register_container_type_mapping(tuple, "tuple", "(", ")")
        self.type_mapper.register_container_type_mapping(dict, "dict", "{", "}", ":")


class JavaSyntax(AbstractLanguageSyntax):

    def __init__(self):
        super().__init__(is_prefix=False,
                         stmt_start_delim="", stmt_end_delim=";",
                         block_start_delim="{", block_end_delim="}",
                         flow_control_test_start_delim="(", flow_control_test_end_delim=")",
                         loop_foreach_keyword=":",
                         arg_delim=",",
                         strongly_typed=True,
                         explicit_rtn=True,
                         has_block_scope=True,
                         ternary_replaces_if_expr=True,
                         function_signature_template="$visibility $rtn_type $func_name($args_start$arg_type $arg_name, $args_end)")

        self.type_mapper.register_none_type_name("null")
        self.type_mapper.register_simple_type_mapping(int,  "Integer")
        self.type_mapper.register_simple_type_mapping(float,  "Float")
        self.type_mapper.register_simple_type_mapping(str,  "String")
        self.type_mapper.register_simple_type_mapping(bool, "Boolean", lambda v: "true" if v else "false")

        self.type_mapper.register_container_type_mapping(
            list,
            "List<?>",
            "new ArrayList<>(List.of(", "))")
        self.type_mapper.register_container_type_mapping(
            tuple,
            "Tuple<?>",
            "Tuple.of(", ")")
        self.type_mapper.register_container_type_mapping(
            dict,
            "Map<?>",
            "new HashMap<>(Map.of(", "))", ",")

        print_fmt = {int: "%d", float: "%d", str: "%s"}
        self.register_function_rewrite(
            py_name="print", py_type=None,
            target_name="System.out.println",
            rewrite=lambda args, rw:
                rw.replace_args_with(
                  rw.call("String.format")
                    .prepend_arg(" ".join([print_fmt[a.type] for a in args]))
                      .append_args(args))
                if len(args) > 1 else None)

        self.register_function_rewrite(
            py_name="len", py_type=str,
            target_name="length",
            rewrite=lambda args, rw:
                rw.rewrite_as_attr_method_call())

        self.register_function_rewrite(
            py_name="len", py_type=list,
            target_name="size",
            rewrite=lambda args, rw:
                rw.rewrite_as_attr_method_call())

        self.register_function_rewrite(
            py_name="len", py_type=tuple,
            target_name="size",
            rewrite=lambda args, rw:
                rw.rewrite_as_attr_method_call())

        self.register_function_rewrite(
            py_name="<>_==", py_type=None,
            rewrite=lambda args, rw:
                rw.replace_node_with(rw.call("equals"))
                  .rewrite_as_attr_method_call() # equals(s s2) -> s.equals(s2)
                if args[0].type == str else None) # only for str...for now FIX

        # str
        self.register_function_rename(py_name="endswith", py_type=str,
                                      target_name="endsWith")
        self.register_function_rename(py_name="startswith", py_type=str,
                                      target_name="startsWith")
        self.register_function_rename(py_name="strip", py_type=str,
                                      target_name="trim")
        self.register_function_rename(py_name="upper", py_type=str,
                                      target_name="toUpperCase")
        self.register_function_rename(py_name="lower", py_type=str,
                                      target_name="toLowerCase")
        self.register_function_rename(py_name="index", py_type=str,
                                      target_name="indexOf")
        self.register_function_rename(py_name="find", py_type=str,
                                      target_name="indexOf")

        self.register_function_rewrite(
            py_name="join", py_type=str, target_name="String.join",
            rewrite=lambda args, rw:
                rw.rewrite_as_func_call(inst_1st=True))

        self.register_function_rewrite(
            py_name="split", py_type=str,
            rewrite=lambda args, rw:
                rw.replace_node_with(rw.call("Arrays.asList"),
                                     current_node_becomes_singleton_arg=True))

        def _slice_rewrite(args, rw):
            if len(args) == 2 and isinstance(args[1].node, ast.UnaryOp):
                lhs = nodebuilder.attr_call(rw.target_node, "length")
                rhs = args[1].node.operand
                binop = nodebuilder.binop("-", lhs, rhs)
                rw.call_on_target("substring", keep_args=False).append_arg(args[0]).append_arg(binop)
            else:
                rw.call_on_target("substring")
        self.register_function_rewrite(
            py_name="<>_[]", py_type=str,
            rewrite=_slice_rewrite)

        # file
        self.type_mapper.register_simple_type_mapping(context.TypeInfo.textiowraper(), "File")
        self.register_function_rewrite(
            py_name="open", py_type=str, target_name="new File",
            rewrite=lambda args, rw: rw.keep_first_arg())

        def _read_rewrite(args, rw, is_readlines):
            rw.rewrite_as_func_call().rename("Files.readString")
            file_arg = rw.wrap(rw.arg_nodes[0])
            rw.replace_args_with(file_arg.chain_method_call("toPath"))
            if is_readlines:
                # in python readlines returns a list of strings
                # so we'll call split("\n")
                rw.chain_method_call("split", args=["\\n"])
                # split returns an Array, so we wrap the whole thing in
                # Arrays.asList
                rw.replace_node_with(rw.call("Arrays.asList"),
                                     current_node_becomes_singleton_arg=True)

        self.register_function_rewrite(
            py_name="read", py_type=context.TypeInfo.textiowraper(),
            rewrite=functools.partial(_read_rewrite, is_readlines=False))

        self.register_function_rewrite(
            py_name="readlines", py_type=context.TypeInfo.textiowraper(),
            rewrite= functools.partial(_read_rewrite, is_readlines=True))

        def _write_rewrite(args, rw):
            rw.rewrite_as_func_call().rename("Files.writeString")
            content_arg = rw.arg_nodes[0]
            file_arg = rw.wrap(rw.arg_nodes[1])
            rw.replace_args_with(
                file_arg.chain_method_call("toPath"))\
                .append_arg(content_arg)\
                .append_arg(rw.ident("Charset.defaultCharset()"))
        self.register_function_rewrite(
            py_name="write", py_type=context.TypeInfo.textiowraper(),
            rewrite=_write_rewrite)


        # list
        self.register_function_rename(py_name="append", py_type=list,
                                      target_name="add")

        self.register_function_rewrite(
            py_name="sort", py_type=list,
            rewrite=lambda args, rw:
                rw.append_arg(rw.ident("null")))

        self.register_function_rewrite(
            py_name="<>_[]", py_type=list,
            rewrite=lambda args, rw: rw.call_on_target("get"))

        # dict
        self.register_function_rewrite(
            py_name="<>_[]", py_type=dict,
            rewrite=lambda args, rw: rw.call_on_target("get"))

        self.register_function_rewrite(
            py_name="<>_dict_assignment", py_type=dict,
            rewrite=lambda args, rw: rw.call_on_target("put"))


class ElispSyntax(AbstractLanguageSyntax):

    def __init__(self):
        super().__init__(is_prefix=True,
                         stmt_start_delim="", stmt_end_delim="",
                         block_start_delim="", block_end_delim="",
                         flow_control_test_start_delim="", flow_control_test_end_delim="",
                         arg_delim=" ",
                         strongly_typed=False,
                         explicit_rtn=False,
                         has_block_scope=False,
                         function_signature_template="(defun2 $func_name ($args_start$arg_name $args_end)")

        self.type_mapper.register_none_type_name("nil")        
        self.type_mapper.register_simple_type_mapping(bool, None, lambda v: "t" if v else "nil")
        self.type_mapper.register_container_type_mapping(list, "list", "(list", ")")
        self.type_mapper.register_container_type_mapping(tuple, "list", "(list", ")")
        self.type_mapper.register_container_type_mapping(dict, "hash-table", "#s(hash-table test equal data (", "))")

        self.register_function_rewrite(
            py_name="append", py_type=list,
            target_name="add-to-list",
            rewrite=lambda args, rw: rw
                .rewrite_as_func_call(inst_1st=True, inst_renamer=lambda v: "'" + v))

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
                            [a if a.type == str else rw.call("int-to-string")
                                .append_arg(a) for a in args]),
                    keep_args=False)
                if args[0].type == str else
                    rw.replace_node_with(rw.call("+")))

        self.register_function_rewrite(
            py_name="<>_*", py_type=None,
            rewrite=lambda args, rw:
                rw.replace_node_with(rw.call("*")))

        self.register_function_rewrite(
            py_name="<>_/", py_type=None,
            rewrite=lambda args, rw:
                rw.replace_node_with(rw.call("/")))

        self.register_function_rewrite(
            py_name="<>_-", py_type=None,
            rewrite=lambda args, rw:
                rw.replace_node_with(rw.call("-")))

        def _defun_rewrite(args, rw):
            f = rw.call("defun").stmt() # stmt so (defun ..) is followed by \n
            f.prepend_arg(rw.ident(rw.node.name))
            if len(args) == 0:
                args_list = rw.call("")
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

        def _for_rewrite(args, rw):
            f = rw.call("dolist").stmt() # stmt so (dolist ..) is followed by \n
            args_list = rw.call(args[0].node.id).append_arg(args[1].node)
            f.append_arg(args_list)
            rw.wrap(rw.node.body[0]).newline().indent_incr()
            rw.wrap(rw.node.body[-1]).newline().indent_decr()
            f.append_args(rw.node.body)
            rw.replace_node_with(f, keep_args=False)
        self.register_function_rewrite(py_name="<>_loop_for", py_type=None, rewrite=_for_rewrite)        

        self.register_function_rewrite(
            py_name="<>_==", py_type=None,
            rewrite=lambda args, rw: rw.replace_node_with(rw.call("equal")))

        # str
        self.register_function_rewrite(
            py_name="endswith", py_type=str, target_name="string-suffix-p",
            rewrite=lambda args, rw: rw.rewrite_as_func_call())

        self.register_function_rewrite(
            py_name="join", py_type=str, target_name="mapconcat",
            rewrite=lambda args, rw: rw.rewrite_as_func_call().prepend_arg(rw.ident("'identity")))

        self.register_function_rewrite(
            py_name="startswith", py_type=str, target_name="string-prefix-p",
            rewrite=lambda args, rw: rw.rewrite_as_func_call())

        self.register_function_rewrite(
            py_name="strip", py_type=str, target_name="string-trim",
            rewrite=lambda args, rw: rw.rewrite_as_func_call())

        self.register_function_rewrite(
            py_name="upper", py_type=str, target_name="upcase",
            rewrite=lambda args, rw: rw.rewrite_as_func_call())

        self.register_function_rewrite(
            py_name="lower", py_type=str, target_name="downcase",
            rewrite=lambda args, rw: rw.rewrite_as_func_call())

        self.register_function_rewrite(
            py_name="split", py_type=str, target_name="split-string",
            rewrite=lambda args, rw: rw.rewrite_as_func_call(inst_1st=True))

        self.register_function_rewrite(
            py_name="index", py_type=str, target_name="cl-search",
            rewrite=lambda args, rw: rw.rewrite_as_func_call(inst_1st=False))

        self.register_function_rewrite(
            py_name="find", py_type=str, target_name="cl-search",
            rewrite=lambda args, rw: rw.rewrite_as_func_call(inst_1st=False))

        self.register_function_rewrite(
            py_name="<>_[]", py_type=str,
            rewrite=lambda args, rw: rw.call_with_target_as_arg("substring"))

        # file
        self.register_function_rewrite(py_name="open", py_type=str,
            rewrite=lambda args, rw: rw.replace_node_with(rw.wrap(args[0].node)))
        def _read_rewrite(args, rw, is_readlines):
            rw.rewrite_as_func_call()            
            f = rw.call("with-temp-buffer")\
                .append_arg(rw.call("insert-file-contents").newline().indent_incr()
                    .append_args(rw.arg_nodes))\
                .append_arg(rw.call("buffer-string").newline().indent_decr())
            if is_readlines:
                f = rw.call("split-string").append_arg(f).append_arg("\\n")
            rw.replace_node_with(f, keep_args=False)

        self.register_function_rewrite(
            py_name="read", py_type=context.TypeInfo.textiowraper(),
            rewrite=functools.partial(_read_rewrite, is_readlines=False))

        self.register_function_rewrite(
            py_name="readlines", py_type=context.TypeInfo.textiowraper(),
            rewrite=functools.partial(_read_rewrite, is_readlines=True))

        self.register_function_rewrite(py_name="write", py_type=context.TypeInfo.textiowraper(),
            rewrite=lambda args, rw:
                rw.rewrite_as_func_call()
                    .replace_node_with(
                        rw.call("with-temp-file")
                            .append_arg(rw.arg_nodes[1])
                            .append_arg(rw.call("insert").append_arg(rw.arg_nodes[0]).newline().indent_incr().indent_decr()),
                        keep_args=False))

        # list
        self.register_function_rename(py_name="len", py_type=None, target_name="length")

        self.register_function_rewrite(
            py_name="<>_[]", py_type=list,
            rewrite=lambda args, rw:
                rw.call_with_target_as_arg("nth", target_as_first_arg=False))

        self.register_function_rewrite(
            py_name="sort", py_type=list,
            rewrite=lambda args, rw:
                rw.rewrite_as_func_call().append_arg(rw.ident("'<")).reassign_to_arg())

        # dict
        self.register_function_rewrite(
            py_name="<>_[]", py_type=dict,
            rewrite=lambda args, rw:
                rw.call_with_target_as_arg("gethash", target_as_first_arg=False))

        self.register_function_rewrite(
            py_name="<>_dict_assignment", py_type=dict,
            rewrite=lambda args, rw:
                rw.call_with_target_as_arg("puthash", target_as_first_arg=False))            

