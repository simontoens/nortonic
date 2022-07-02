import asttoken
import context
import function


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
    pass


class CommonInfixFormatter(AbstractLanguageFormatter):

    def delim_suffix(self, token, remaining_tokens):
        if asttoken.next_token_has_type(remaining_tokens, asttoken.TARGET_DEREF):
            # no space before '.': "foo".startswith("f"), not "foo" .startswith
            return False
        if token.type.is_target_deref:
            # no space after '.': "foo".startswith("f")
            return False
        if token.type.is_func_call:
            # no space after func name: print("foo", ... - not print( "foo", ...
            return False
        if token.type.is_func_call_boundary and token.is_end and asttoken.next_token_has_value(remaining_tokens):
            # "foo".length() == 3, not "foo".length()== 3;
            return True
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
        return True



class AbstractLanguageSyntax:
    """
    Stateless metadata that describes a Language Syntax.

    TODO instead of start/end delim, refer to a character family, like parens,
    curlys etc
    """

    def __init__(self, is_prefix,
                 stmt_start_delim, stmt_end_delim,
                 block_start_delim, block_end_delim,
                 flow_control_test_start_delim, flow_control_test_end_delim,
                 loop_foreach_keyword,
                 arg_delim,
                 strongly_typed,
                 explicit_rtn,
                 has_block_scope,
                 function_signature_template):
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
                      .append_args([a.node for a in args]))
                if len(args) > 1 else None)

        self.register_function_rewrite(
            py_name="len", py_type=None,
            target_name="length",
            rewrite=lambda args, rw:
                rw.rewrite_as_attr_method_call())

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
        self.register_function_rename(py_name="append", py_type=list,
                                      target_name="add")

        self.register_function_rewrite(
            py_name="<>_[]", py_type=list,
            rewrite=lambda args, rw:
                rw.replace_node_with(rw.call("get"))
                  .rewrite_as_attr_method_call())

        self.register_function_rewrite(
            py_name="<>_[]", py_type=dict,
            rewrite=lambda args, rw:
                rw.replace_node_with(rw.call("get"))
                  .rewrite_as_attr_method_call())

        self.register_function_rewrite(
            py_name="<>_dict_assignment", py_type=dict,
            rewrite=lambda args, rw:
                rw.replace_node_with(rw.call("put").stmt())
                  .rewrite_as_attr_method_call())

class ElispSyntax(AbstractLanguageSyntax):

    def __init__(self):
        super().__init__(is_prefix=True,
                         stmt_start_delim="", stmt_end_delim="",
                         block_start_delim="", block_end_delim="",
                         flow_control_test_start_delim="", flow_control_test_end_delim="",
                         loop_foreach_keyword="???",
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

        self.register_function_rewrite(
            py_name="<>_[]", py_type=list,
            rewrite=lambda args, rw:
                rw.replace_node_with(rw.call("nth")
                    .append_args(list(reversed([a.node for a in args]))),
                keep_args=False))

        self.register_function_rewrite(
            py_name="<>_[]", py_type=dict,
            rewrite=lambda args, rw:
                rw.replace_node_with(rw.call("gethash")
                    .append_args(list(reversed([a.node for a in args]))),
                keep_args=False))

        self.register_function_rewrite(
            py_name="<>_dict_assignment", py_type=dict,
            rewrite=lambda args, rw:
                rw.replace_node_with(rw.call("puthash")
                    .append_args([args[1].node, args[2].node, args[0].node]),
                keep_args=False))
