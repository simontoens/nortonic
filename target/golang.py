from target.targetlanguage import AbstractTargetLanguage
from target.targetlanguage import CommonInfixFormatter
from target.targetlanguage import NodeVisitor
import ast
import asttoken
import context
import functools
import nodeattrs
import nodebuilder
import nodes
import templates
import types
import visitor.visitor as visitor
import visitor.visitors as visitors


EXPLICIT_TYPE_DECLARATION_NULL_RHS = "golang__explicit_type_decl_rhs"
# placeholder value for '\n' (instead of "\n")
SINGLE_QUOTE_LINE_BREAK_CHAR = "golang__single_quote_line_break"
REQUIRES_ERROR_HANDLING = "golang__requires_error_handling"


class GolangTypeDeclarationTemplate(templates.TypeDeclarationTemplate):

    def __init__(self):
        super().__init__("$identifier := $rhs")

    def pre_render__hook(self, declaration, scope, node_attrs):
        if EXPLICIT_TYPE_DECLARATION_NULL_RHS in node_attrs:
            return "var $identifier $type"
        else:
            return declaration

    def post_render__hook(self, declaration, scope, node_attrs):
        # when the "discarding identifier" '_' is used, assignment has to be
        # "=", never ":="
        bad_discarding_form = "_ :="
        if declaration.startswith(bad_discarding_form):
            return declaration.replace(bad_discarding_form, "_ =")
        else:
            return declaration


class GolangFunctionSignatureTemplate(templates.FunctionSignatureTemplate):

    def __init__(self, is_anon):
        if is_anon:
            s = "func($args_start$arg_name $arg_type, $args_end) $rtn_type"
        else:
            s = "func $func_name($args_start$arg_name $arg_type, $args_end) $rtn_type"
        super().__init__(s)

    def post_render__hook(self, signature, function_name, arguments, scope):
        """
        This hook impl removes repeated types in the method signature, one
        of the more sugary of Golangs syntactic sugars:
          func foo(s1 string, s2 string, s3 string)
        ->
          func foo(s1, s2, s3 string)
        """
        for i, argument in enumerate(arguments):
            arg_name, arg_type_name = argument
            if i < len(arguments) - 1:
                next_arg_type_name = arguments[i + 1][1]
                if arg_type_name == next_arg_type_name:
                    arg_and_type_name = "%s %s" % (arg_name, arg_type_name)
                    i = signature.index(arg_and_type_name)
                    signature = signature.replace(arg_and_type_name, arg_name)
        return signature


class GolangSyntax(AbstractTargetLanguage):

    def __init__(self):
        super().__init__(formatter=GolangFormatter(),
                         is_prefix=False,
                         stmt_end_delim=";", stmt_end_delim_always_required=False,
                         block_start_delim="{", block_end_delim="}",
                         flow_control_test_start_delim="",
                         flow_control_test_end_delim="",
                         arg_delim=",",
                         has_block_scope=True,
                         has_assignment_lhs_unpacking=False,
                         type_declaration_template=GolangTypeDeclarationTemplate(),
                         anon_function_signature_template=GolangFunctionSignatureTemplate(is_anon=True),
                         function_signature_template=GolangFunctionSignatureTemplate(is_anon=False),
                         function_can_return_multiple_values=True,
                         has_pointers=True)

        self.type_mapper.register_none_type_name("nil")
        self.type_mapper.register_simple_type_mapping(bool,"bool", lambda v: "true" if v else "false")
        self.type_mapper.register_simple_type_mapping(int, "int")
        self.type_mapper.register_simple_type_mapping(str, "string")
        self.type_mapper.register_simple_type_mapping(float, "float32")
        self.type_mapper.register_simple_type_mapping(bytes, "[]byte")

        self.type_mapper.register_function_type_mapping(GolangFunctionSignatureTemplate(is_anon=True))

        self.type_mapper.register_container_type_mapping(
            list,
            "[]$contained_type$[0]",
            start_literal="[]$contained_type$[0]{",
            end_literal="}")

        # TODO - review best way to represent a tuple in Golang
        self.type_mapper.register_container_type_mapping(
            tuple,
            "[]Tuple",
            start_literal="[]$contained_type$[]{",
            end_literal="}")

        # homogeneous tuple - make this a slice
        self.type_mapper.register_container_type_mapping(
            tuple,
            "[]$contained_type$[0]",
            start_literal="[]$contained_type$[0]{",
            end_literal="}",
            apply_if=lambda type_info: type_info.contains_homogeneous_types)

        map_decl = "map[$contained_type$[0]]$contained_type$[1]"
        self.type_mapper.register_container_type_mapping(
            dict,
            map_decl,
            start_literal="%s{" % map_decl,
            end_literal="}",
            values_separator=":")

        self.type_mapper.register_type_coercion_rule(str, int, str, "string")
        self.type_mapper.register_type_coercion_rule(str, float, str, "string")

        def _visit_assignment(args, rw):
            rhs_arg = args[1]
            if rhs_arg.type is type(None):
                # this case doesnt' work:
                # i = None
                # i = 1
                # i = None
                # ... unless we make every type a pointer...
                rw.set_node_attr(EXPLICIT_TYPE_DECLARATION_NULL_RHS)
                
        self.register_function_rewrite(py_name="<>_=", py_type=None,
                                       rewrite=_visit_assignment)

        def _rewrite_str_mod(args, rw):
            format_call = rw.call("fmt.Sprintf")
            keep_args = True
            rhs = args[1]
            if rhs.type is tuple:
                keep_args = False
                format_call.append_arg(args[0])
                format_call.append_args(rhs.node.elts)
            rw.replace_node_with(format_call, keep_args)
        self.register_function_rewrite(
            py_name="<>_%", py_type=str,
            rewrite=_rewrite_str_mod)

        self.register_function_rewrite(
            py_name="<>_loop_for", py_type=None, rewrite=lambda args, rw:
                rw.rewrite_as_c_style_loop())

        self.register_function_rename(py_name="print", py_type=None,
                                      target_name="fmt.Println")

        # str
        self.register_function_rename(py_name="str", py_type=None,
                                      target_name="string")

        self.register_function_rewrite(
            py_name="lower", py_type=str, target_name="strings.ToLower",
            rewrite=lambda args, rw: rw.rewrite_as_func_call())

        self.register_function_rewrite(
            py_name="upper", py_type=str, target_name="strings.ToUpper",
            rewrite=lambda args, rw: rw.rewrite_as_func_call())
        
        self.register_function_rewrite(
            py_name="startswith", py_type=str, target_name="strings.HasPrefix",
            rewrite=lambda args, rw: rw.rewrite_as_func_call(inst_1st=True))
        
        self.register_function_rewrite(
            py_name="endswith", py_type=str, target_name="strings.HasSuffix",
            rewrite=lambda args, rw: rw.rewrite_as_func_call(inst_1st=True))

        self.register_function_rewrite(
            py_name="strip", py_type=str, target_name="strings.TrimSpace",
            rewrite=lambda args, rw: rw.rewrite_as_func_call())

        self.register_function_rewrite(
            py_name="join", py_type=str, target_name="strings.Join",
            rewrite=lambda args, rw: rw.rewrite_as_func_call(inst_1st=False))

        self.register_function_rewrite(
            py_name="split", py_type=str, target_name="strings.Split",
            rewrite=lambda args, rw:
                # python has split(), which splits in whitespace
                rw.rewrite_as_func_call(inst_1st=True).append_arg(" ")
                if len(args) == 0 else rw.rewrite_as_func_call(inst_1st=True))

        self.register_function_rewrite(
            py_name="index", py_type=str, target_name="strings.Index",
            rewrite=lambda args, rw: rw.rewrite_as_func_call(inst_1st=True))

        self.register_function_rewrite(
            py_name="find", py_type=str, target_name="strings.Index",
            rewrite=lambda args, rw: rw.rewrite_as_func_call(inst_1st=True))

        def _slice_rewrite(args, rw):
            if len(args) == 2 and isinstance(args[1].node, ast.UnaryOp):
                lhs = rw.call(context.LEN_BUILTIN).append_arg(rw.target_node)
                rhs = args[1].node.operand
                binop = rw.binop("-", lhs, rhs)
                setattr(args[1].node, nodeattrs.ALT_NODE_ATTR, binop.node)
        self.register_function_rewrite(
            py_name="<>_[]", py_type=str,
            rewrite=_slice_rewrite)

        # input
        self.register_function_rewrite(
            py_name="input", py_type=str,
            target_name="bufio.NewReader(os.Stdin).ReadString",
            rewrite=lambda args, rw:
                rw.insert_above(rw.call(context.PRINT_BUILTIN).append_arg(args[0]))
                  .replace_args_with(SINGLE_QUOTE_LINE_BREAK_CHAR))

        # list
        self.register_function_rewrite(
            py_name="append", py_type=list,
            rewrite=lambda args, rw: rw
                .rewrite_as_func_call(inst_1st=True)
                .reassign_to_arg())

        self.register_function_rewrite(
            py_name="sort", py_type=list,
            target_name="sort.Slice",
            rewrite=lambda args, rw: rw
                .rewrite_as_func_call()
                .append_arg(rw.funcdef_lambda(
                    args=[rw.ident("i"), rw.ident("j")],
                    body=rw.compare(
                        "<",
                        rw.subscript_list(rw.target_node, rw.ident("i")),
                        rw.subscript_list(rw.target_node, rw.ident("j"))))))


        # file
        self.type_mapper.register_simple_type_mapping(context.TypeInfo.textiowraper(), "os.File")

        def _open_rewrite(args, rw):
            rw.set_node_attr(REQUIRES_ERROR_HANDLING)
            rw.remove_args()
            rw.append_arg(args[0])
            is_write_mode =\
                (len(args) > 1 and
                 isinstance(args[1].node, ast.Constant) and
                 ("w" in args[1].node.value or "r+" in args[1].node.value))
            if is_write_mode:
                rw.rename("os.Create")
            else:
                rw.rename("os.Open")

        self.register_function_rewrite(
            py_name="open", py_type=str, target_name="os.Open",
            rewrite=_open_rewrite)

        def _read_rewrite(args, rw, is_readlines):
            readfile_call = rw.call("os.ReadFile", bytes)\
                .set_node_attr(REQUIRES_ERROR_HANDLING)\
                .append_arg(rw.target.chain_method_call("Name"))
            root_node = rw.call("string", context.TypeInfo.str())\
                .append_arg(readfile_call)
            if is_readlines:
                root_node = rw.call("strings.Split")\
                    .append_arg(root_node).append_arg("\\n")
            return rw.replace_node_with(root_node)

        self.register_function_rewrite(
            py_name="read", py_type=context.TypeInfo.textiowraper(),
            rewrite=functools.partial(_read_rewrite, is_readlines=False))

        self.register_function_rewrite(
            py_name="readlines", py_type=context.TypeInfo.textiowraper(),
            rewrite=functools.partial(_read_rewrite, is_readlines=True))

        self.register_function_rewrite(
            py_name="write", py_type=context.TypeInfo.textiowraper(),
            target_name="os.WriteFile",
            rewrite=lambda args, rw:
                rw.set_node_attr(REQUIRES_ERROR_HANDLING)
                    .rewrite_as_func_call().remove_args()
                    .append_arg(rw.target.chain_method_call("Name"))
                    .append_arg(rw.xcall("[]byte").append_arg(args[0]))
                    .append_arg(rw.xident("0644")))

        def _rewrite_sep(args, rw):
            rw.replace_node_with(rw.call(context.STR_BUILTIN).append_arg(
                    rw.xident("os.PathSeparator")))            
            
        self.register_attribute_rewrite(
            py_name="sep", py_type=context.TypeInfo.module("os"),
            rewrite=_rewrite_sep)

        self.register_attribute_rewrite(
            py_name="sep", py_type=context.TypeInfo.module("os.path"),
            rewrite=_rewrite_sep)

        self.register_function_rewrite(
            py_name="join", py_type=context.TypeInfo.module("os.path"),
            rewrite=lambda args, rw:
                rw.replace_node_with(rw.call("filepath.Join")))
        

        self.register_node_visitor(ErrorNodeVisitor())

    def to_literal(self, value):
        v = super().to_literal(value)
        if isinstance(v, str):
            # this is a bit weird but we sometimes need '\n' instead of "\n"
            if v == '"%s"' % SINGLE_QUOTE_LINE_BREAK_CHAR:
                return "'\\n'"
        return v


class GolangFormatter(CommonInfixFormatter):

    def delim_suffix(self, token, remaining_tokens):
        return super().delim_suffix(token, remaining_tokens)


class ErrorNodeVisitor(visitors.BodyParentNodeVisitor):
    """
    This visitors rewrites function calls that return an Error alongside their
    "normal" return type.
    This visitor is not generic because this style of returning an Error is
    a Golang'ism.

    Currently this visitor only considers the rhs of assign nodes - this isn't
    enough. For example, nested calls also have to re-written:
        my_func(open("path/to/file"))
    ->
        f, _ := os.Open("path/to/file")
        my_func(f)

    The other obvious issue here is that the Error that is returned is just
    swallowed.
    """

    def __init__(self):
        super().__init__()
        self.context = None

    def assign(self, node, num_children_visited):
        super().assign(node, num_children_visited)
        if num_children_visited == -1:
            self._extract_variables(node)

    def expr(self, node, num_children_visited):
        super().expr(node, num_children_visited)
        if num_children_visited == -1:
            self._extract_variables(node.value)            

    def _extract_variables(self, start_node):
        """
        Issues with this logic overall:
          - are errors always the last return type?
        """
        assert self.context is not None
        collector = visitors.NodeCollectingVisitor(
            lambda n: nodeattrs.has_attr(n, REQUIRES_ERROR_HANDLING))
        visitor.visit(start_node, collector)
        if len(collector.nodes) > 0:
            # we need to rewrite as follows, given that os.ReadFile
            # returns ([]byte, error) - for now we swallow the error (1):
            #   lines := strings.Split(string(os.ReadFile(...)), '\n')
            # ->
            #   t, _ := os.ReadFile(...)
            #   lines := strings.Split(string(t), '\n')
            #
            # However, we can skip the "pull up" logic for this format (2):
            #   f := os.Open("/Users/stoens/text.txt")
            # -> this just needs an updated lhs
            #   f, _ := os.Open("/Users/stoens/text.txt")
            #
            # This example shows why we need to loop over all discovered
            # nodes - given this pseudo-Golang:
            # c := string(os.ReadFile(os.Open("a/b/c")))
            # it needs to be rewritten as follows:
            # (1st iteration)
            # -> t, _ = os.Open("a/b/c")
            # -> c := string(os.ReadFile(t.Name()))
            # (2nd iteration)
            # -> t, _ := os.Open("a/b/c")
            # -> t1, _ := os.ReadFile(t.Name())
            # -> c := string(t1)
            for target_node in collector.nodes:
                nodeattrs.rm_attr(target_node, REQUIRES_ERROR_HANDLING)
                if isinstance(start_node, ast.Assign) and target_node is start_node.value:
                    # this is (2) in the comment above
                    rhs = start_node.value
                    rhs_ti = self.context.get_type_info_by_node(rhs)
                    rtn_type = _build_rtn_type_with_error(rhs_ti)
                    nodeattrs.set_type_info(rhs, rtn_type, allow_reset=True)
                    lhs = start_node.targets[0]
                    assert isinstance(lhs, ast.Name)
                    lhs_tuple = nodebuilder.tuple(lhs, "_")
                    assign_node = nodebuilder.assignment(lhs_tuple, rhs)
                    setattr(start_node, nodeattrs.ALT_NODE_ATTR, assign_node)
                else:
                    # this is (1) in the comment above
                    node_to_extract = nodes.shallow_copy_node(
                        target_node, self.context)
                    node_to_extract_ti = self.context.get_type_info_by_node(
                        node_to_extract)
                    # add error type to type
                    rtn_type = _build_rtn_type_with_error(node_to_extract_ti)
                    nodeattrs.set_type_info(node_to_extract, rtn_type, allow_reset=True)
                    ignore_returned_value = not rtn_type.is_sequence
                    if ignore_returned_value:
                        # single rtn type that we should ignore
                        #   os.WriteFile(f.Name() ...
                        # ->
                        #   _ = os.WriteFile(f.Name() ...
                        lhs = nodebuilder.identifier("_")
                        assign_node = nodebuilder.assignment(lhs, node_to_extract)
                        setattr(start_node, nodeattrs.ALT_NODE_ATTR, assign_node)
                    else:
                        # new lhs for extracted variable
                        ident_name = self.context.get_unique_identifier_name()
                        lhs = nodebuilder.tuple(ident_name, "_")
                        assign_node = nodebuilder.assignment(lhs, node_to_extract)
                        nodes.insert_node_above(assign_node, self.parent_body, start_node)
                        setattr(target_node, nodeattrs.ALT_NODE_ATTR, nodebuilder.identifier(ident_name))


def _build_rtn_type_with_error(org_rtn_type):
    if org_rtn_type.is_none_type:
        return context.TypeInfo(Exception)
    else:
        return context.TypeInfo.tuple().of(org_rtn_type, Exception)
