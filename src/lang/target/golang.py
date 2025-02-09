import ast
import copy
import functools
import lang.internal.typeinfo as ti
import lang.nodebuilder as nodebuilder
import lang.nodes as nodes
import lang.target.rewrite as rewrite
import lang.target.targetlanguage as targetlanguage
import lang.target.templates as templates
import os
import visitor.asttoken as asttoken
import visitor.nodeattrs as nodeattrs
import visitor.visitor as visitor
import visitor.visitors as visitors


class GolangSyntax(targetlanguage.AbstractTargetLanguage):

    def __init__(self):
        super().__init__(formatter=targetlanguage.CommonInfixFormatter(),
                         is_prefix=False,
                         stmt_end_delim=";", stmt_end_delim_always_required=False,
                         block_start_delim="{", block_end_delim="}",
                         flow_control_test_start_delim="",
                         flow_control_test_end_delim="",
                         arg_delim=",",
                         has_block_scope=True,
                         has_assignment_lhs_unpacking=False,
                         object_instantiation_arg_delims="{}", # struct
                         type_declaration_template=_GolangTypeDeclarationTemplate(),
                         class_declaration_template="type $class_name struct",
                         anon_function_signature_template=_GolangFunctionSignatureTemplate(is_anon=True),
                         function_signature_template=_GolangFunctionSignatureTemplate(is_anon=False),
                         function_can_return_multiple_values=True,
                         pointer_types=(list, str))

        self.type_mapper.register_none_type_name("nil")
        self.type_mapper.register_simple_type_mapping(bool,"bool", lambda v: "true" if v else "false")
        self.type_mapper.register_simple_type_mapping(int, "int")
        self.type_mapper.register_simple_type_mapping(str, "string")
        self.type_mapper.register_simple_type_mapping(float, "float32")
        self.type_mapper.register_simple_type_mapping(bytes, "[]byte")
        self.type_mapper.register_simple_type_mapping(Exception, "error")

        self.type_mapper.register_function_type_mapping(_GolangFunctionSignatureTemplate(is_anon=True))

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
            homogenous_types=True)

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
                rw.set_node_attr(_EXPLICIT_TYPE_DECLARATION_NULL_RHS)
                
        self.register_rewrite(rewrite.Operator.ASSIGNMENT,
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
        self.register_rewrite(rewrite.Operator.MOD, arg_type=str,
                              rewrite=_rewrite_str_mod)

        self.register_rewrite(rewrite.Keyword.FOR, rewrite=lambda args, rw:
                rw.rewrite_as_c_style_loop())

        self.register_rename("print", to="fmt.Println")

        # str
        self.register_rename("str", to="string")

        self.register_rewrite(str.lower, rename_to="strings.ToLower",
            rewrite=lambda args, rw: rw.rewrite_as_func_call())

        self.register_rewrite(str.upper, rename_to="strings.ToUpper",
            rewrite=lambda args, rw: rw.rewrite_as_func_call())
        
        self.register_rewrite(str.startswith, rename_to="strings.HasPrefix",
            rewrite=lambda args, rw: rw.rewrite_as_func_call(inst_1st=True))
        
        self.register_rewrite(str.endswith, rename_to="strings.HasSuffix",
            rewrite=lambda args, rw: rw.rewrite_as_func_call(inst_1st=True))

        self.register_rewrite(str.strip, rename_to="strings.TrimSpace",
            rewrite=lambda args, rw: rw.rewrite_as_func_call())

        self.register_rewrite(str.join, rename_to="strings.Join",
            rewrite=lambda args, rw: rw.rewrite_as_func_call(inst_1st=False))

        self.register_rewrite(str.split, rename_to="strings.Split",
            rewrite=lambda args, rw:
                # python has split(), which splits in whitespace
                rw.rewrite_as_func_call(inst_1st=True).append_arg(" ")
                if len(args) == 0 else rw.rewrite_as_func_call(inst_1st=True))

        self.register_rewrite(str.index, rename_to="strings.Index",
            rewrite=lambda args, rw: rw.rewrite_as_func_call(inst_1st=True))

        self.register_rewrite(str.find, rename_to="strings.Index",
            rewrite=lambda args, rw: rw.rewrite_as_func_call(inst_1st=True))

        def _slice_rewrite(args, rw):
            if len(args) == 2 and isinstance(args[1].node, ast.UnaryOp):
                lhs = rw.call(len).append_arg(rw.target_node)
                rhs = args[1].node.operand
                binop = rw.binop("-", lhs, rhs)
                setattr(args[1].node, nodeattrs.ALT_NODE_ATTR, binop.node)
        self.register_rewrite(rewrite.Operator.SUBSCRIPT,
            arg_type=str, rewrite=_slice_rewrite)

        # input
        self.register_rewrite(input, arg_type=str,
            rename_to="bufio.NewReader(os.Stdin).ReadString",
            rewrite=lambda args, rw:
                rw.insert_above(rw.call(print).append_arg(args[0]))
                  .replace_args_with(_SINGLE_QUOTE_LINE_BREAK_CHAR))

        # list
        self.register_rewrite(list.append,
            rewrite=lambda args, rw: rw
                .rewrite_as_func_call(inst_1st=True)
                .reassign_to_arg())

        self.register_rewrite(list.sort, rename_to="sort.Slice",
            rewrite=lambda args, rw: rw
                .rewrite_as_func_call()
                .append_arg(rw.funcdef_lambda(
                    args=[rw.ident("i"), rw.ident("j")],
                    body=rw.compare(
                        "<",
                        rw.subscript_list(rw.target_node, rw.ident("i")),
                        rw.subscript_list(rw.target_node, rw.ident("j"))))))


        # file
        self.type_mapper.register_simple_type_mapping(ti.TypeInfo.textiowraper(), "os.File")

        def _open_rewrite(args, rw):
            rw.set_node_attr(_REQUIRES_ERROR_HANDLING)
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

        self.register_rewrite(open, arg_type=str, rename_to="os.Open",
            rewrite=_open_rewrite)

        def _read_rewrite(args, rw, is_readlines):
            readfile_call = rw.call("os.ReadFile", bytes)\
                .set_node_attr(_REQUIRES_ERROR_HANDLING)\
                .append_arg(rw.target.chain_method_call("Name"))
            root_node = rw.call("string", ti.TypeInfo.str())\
                .append_arg(readfile_call)
            if is_readlines:
                root_node = rw.call("strings.Split")\
                    .append_arg(root_node).append_arg("\\n")
            return rw.replace_node_with(root_node)

        self.register_rewrite("read",
            inst_type=ti.TypeInfo.textiowraper(),
            rewrite=functools.partial(_read_rewrite, is_readlines=False))

        self.register_rewrite("readlines",
            inst_type=ti.TypeInfo.textiowraper(),
            rewrite=functools.partial(_read_rewrite, is_readlines=True))

        self.register_rewrite("write", inst_type=ti.TypeInfo.textiowraper(),
            rename_to="os.WriteFile",
            rewrite=lambda args, rw:
                rw.set_node_attr(_REQUIRES_ERROR_HANDLING)
                    .rewrite_as_func_call().remove_args()
                    .append_arg(rw.target.chain_method_call("Name"))
                    .append_arg(rw.xcall("[]byte").append_arg(args[0]))
                    .append_arg(rw.xident("0644")))

        def _rewrite_sep(args, rw):
            rw.replace_node_with(rw.call(str).append_arg(
                    rw.xident("os.PathSeparator")))

        self.register_attr_rewrite("sep", os, _rewrite_sep)

        self.register_attr_rewrite("sep", os.path, _rewrite_sep)

        self.register_rewrite(os.path.join, imports="path/filepath",
            rewrite=lambda args, rw:
                rw.replace_node_with(rw.call("filepath.Join")))
        

        self.register_node_visitor(_AppendArgumentVisitor())
        self.register_node_visitor(_ErrorNodeVisitor())
        self.register_destructive_node_visitor(_PullMethodDeclarationsOutOfClass())

    def to_literal(self, value):
        v = super().to_literal(value)
        if isinstance(v, str):
            # this is a bit weird but we sometimes need '\n' instead of "\n"
            if v == '"%s"' % _SINGLE_QUOTE_LINE_BREAK_CHAR:
                return "'\\n'"
        return v


_EXPLICIT_TYPE_DECLARATION_NULL_RHS = "golang__explicit_type_decl_rhs"
# placeholder value for '\n' (instead of "\n")
_SINGLE_QUOTE_LINE_BREAK_CHAR = "golang__single_quote_line_break"
_REQUIRES_ERROR_HANDLING = "golang__requires_error_handling"
_RECEIVER_TYPE = "golang__receiver_type"


class _GolangTypeDeclarationTemplate(templates.TypeDeclarationTemplate):

    def __init__(self):
        super().__init__("$identifier := $rhs")

    def pre_render__hook(self, declaration, scope, node_attrs):
        class_name, _ = scope.get_enclosing_class()
        if class_name is None:
            if _EXPLICIT_TYPE_DECLARATION_NULL_RHS in node_attrs:
                return "var $identifier $type"
            else:
                return declaration
        else:
            # within a class -> so a struct, really
            return "$identifier $type"

    def post_render__hook(self, declaration, scope, node_attrs):
        # when the "discarding identifier" '_' is used, assignment has to be
        # "=", never ":="
        # update - we now throw these assignments away, see unpacking rewrite
        # logic in visitors
        bad_discarding_form = "_ :="
        if declaration.startswith(bad_discarding_form):
            return declaration.replace(bad_discarding_form, "_ =")
        else:
            return declaration


class _GolangFunctionSignatureTemplate(templates.FunctionSignatureTemplate):

    def __init__(self, is_anon):
        if is_anon:
            s = "func($args_start$arg_name $arg_type, $args_end) $rtn_type"
        else:
            s = " $func_name($args_start$arg_name $arg_type, $args_end) $rtn_type"
        super().__init__(s)
        self.is_anon = is_anon

    def post_render__hook(self, signature, function_name, arguments, scope, node_attrs):
        # remove repeated types in signature:
        # func f(s1 string, s2 string, s3 string) -> func f(s1, s2, s3 string)
        for i, argument in enumerate(arguments):
            arg_name, arg_type_name = argument
            if i < len(arguments) - 1:
                next_arg_type_name = arguments[i + 1][1]
                if arg_type_name == next_arg_type_name:
                    arg_and_type_name = "%s %s" % (arg_name, arg_type_name)
                    i = signature.index(arg_and_type_name)
                    signature = signature.replace(arg_and_type_name, arg_name)

        if not self.is_anon:
            receiver_type = node_attrs.get(_RECEIVER_TYPE)
            if receiver_type is not None:
                signature = "(self *%s) " % receiver_type + signature
            signature = "func " + signature

        return signature


class _AppendArgumentVisitor(visitor.NoopNodeVisitor):    
    """
    If append is called for a slicce of pointers, if the "item  to add" argument
    is a pointer, do not dereference it.

    There's currently no good way to do this generically, as it is target
    language dependent.
    """
    def __init__(self):
        super().__init__()
        self.context = None # initialized when this instance is used

    def call(self, node, num_children_visited):
        super().call(node, num_children_visited)
        if num_children_visited == -1:
            # not called on an instance, to make sure we are messing with right
            # "append" function
            if isinstance(node.func, ast.Name):
                func = nodeattrs.get_function(node)
                if func.is_builtin and func.name == "append":
                    slice_ti = self.context.get_type_info_by_node(node.args[0].get())
                    if slice_ti.get_contained_type_info_at(0).is_pointer:
                        # this is a slice of pointers, so we should not
                        # deref the pointer of the 2nd argumemnt, the item to
                        # append to the list
                        item_arg_node = node.args[1].get()
                        item_ti = self.context.get_type_info_by_node(item_arg_node)
                        if item_ti.is_pointer:
                            if nodeattrs.has_attr(item_arg_node, nodeattrs.DEREF_NODE_MD):
                                nodeattrs.rm_attr(item_arg_node, nodeattrs.DEREF_NODE_MD)


class _ErrorNodeVisitor(visitors.BodyParentNodeVisitor):
    """
    This visitors rewrites function calls that return an Error alongside their
    "normal" return type.
    This visitor is not generic because this style of returning an Error is
    a Golang'ism.

    lines := strings.Split(string(os.ReadFile(...)), '\n')
      ->
    t, _ := os.ReadFile(...)
    lines := strings.Split(string(t), '\n')
    """

    def __init__(self):
        super().__init__()
        self.context = None # initialized when this instance is used

    def assign(self, node, num_children_visited):
        super().assign(node, num_children_visited)
        if num_children_visited == -1:
            self._extract_and_add_discarded_error(node)

    def expr(self, node, num_children_visited):
        super().expr(node, num_children_visited)
        if num_children_visited == -1:
            self._extract_and_add_discarded_error(node.value)
            if nodeattrs.has_attr(node.value, _REQUIRES_ERROR_HANDLING):
                # for example:
                #   os.WriteFile(f.Name(), []byte(content), 0644)
                # ->
                #   _ := os.WriteFile(f.Name(), []byte(content), 0644)
                nodeattrs.rm_attr(node.value, _REQUIRES_ERROR_HANDLING)
                assign_node = nodebuilder.assignment("_", node.value)
                _add_error_to_lhs(assign_node, self.context)
                setattr(node, nodeattrs.ALT_NODE_ATTR, assign_node)

    def _extract_and_add_discarded_error(self, start_node):
        """
        Are errors always the last return type?
        """
        assert self.context is not None
        assign_nodes = nodes.extract_expressions_with_attr(
            start_node, self.parent_body, _REQUIRES_ERROR_HANDLING, self.context,
            remove_attr=True)
        for assign_node in assign_nodes:
            _add_error_to_lhs(assign_node, self.context)


def _add_error_to_lhs(assign_node, context):
    lhs = assign_node.targets[0]
    assert isinstance(lhs, ast.Name)
    rhs = assign_node.value
    rhs_ti = context.get_type_info_by_node(rhs)
    rtn_type = _build_rtn_type_with_error(rhs_ti)
    nodeattrs.set_type_info(rhs, rtn_type, allow_reset=True)
    ignore_rtn_value = not rtn_type.is_sequence
    if ignore_rtn_value:
        # single rtn type, must be the Error, so ignore it
        #   t1 := os.WriteFile(...)
        # ->
        #   _ = os.WriteFile(...)
        discard_id = nodebuilder.identifier("_")
        setattr(lhs, nodeattrs.ALT_NODE_ATTR, discard_id)
    else:
        # Add error as a 2nd, ignored return value
        #   t1 := os.Open(...)
        # ->
        #   t1, _ := os.Open(f.Name() ...
        alt_lhs = nodebuilder.tuple(lhs.id, "_")
        setattr(lhs, nodeattrs.ALT_NODE_ATTR, alt_lhs)


def _build_rtn_type_with_error(org_rtn_type):
    if org_rtn_type.is_none_type:
        return ti.TypeInfo(Exception)
    else:
        return ti.TypeInfo.tuple().of(org_rtn_type, Exception)


class _PullMethodDeclarationsOutOfClass(visitors.BodyParentNodeVisitor):
    """
    This visitors pulls class methods out of the class sccope.
    It also replaces the ctor with a New<type> function.
    """

    def __init__(self):
        super().__init__()
        self.current_class_node = None
        self.context = None # initialized when this instance is used

    def classdef(self, node, num_children_visited):
        if num_children_visited == 0:
            # or get it from the scope, which would also work for nested classes
            # of course to support nested classes, more work would have to be
            # done anyway
            self.current_class_node = node
        elif num_children_visited == -1:
            self.current_class_node = None

    def call(self, node, num_children_visited):
        super().call(node, num_children_visited)
        if num_children_visited == -1:
            func = nodeattrs.get_function(node)
            if func.is_constructor:
                # switch callsite to use New<type>
                node.func.id = self._get_new_func_name(func.get_rtn_type_info())
                func = copy.copy(func)
                nodeattrs.set_function(node, func, allow_reset=True)
                func.is_constructor = False
                
            
    def funcdef(self, node, num_children_visited):
        super().funcdef(node, num_children_visited)
        if num_children_visited == -1:
            func = nodeattrs.get_function(node)
            if func.is_method:
                assert self.current_class_node is not None
                receiver_ti = self.context.get_type_info_by_node(self.current_class_node)
                func_def_node = nodes.shallow_copy_node(node, self.context)

                # parent_body -> class
                # grandparent_boby -> where class is defined (module typically)
                nodes.insert_node_below(
                    func_def_node,
                    self.grandparent_body,
                    self.current_class_node)
                nodeattrs.skip(node)

                if func.is_constructor:
                    # we replace __init__ with a simple function called
                    # New<type>
                    func_def_node.name = self._get_new_func_name(receiver_ti)
                    # ctor body:
                    # since "self" is used to reference the instance, we keep
                    # that ident name. probably this would be nice clean up
                    instance = nodebuilder.identifier("self")
                    self.context.register_type_info_by_node(instance, receiver_ti)
                    new_struct = nodebuilder.call(receiver_ti.name)
                    nodeattrs.set_function(new_struct, copy.copy(func))
                    self.context.register_type_info_by_node(new_struct, receiver_ti)
                    assign_node = nodebuilder.assignment(instance, new_struct)
                    func_def_node.body.insert(0, assign_node)
                    # return self
                    addr_of_instance = nodes.shallow_copy_node(instance, self.context)
                    nodeattrs.set_attr(addr_of_instance, nodeattrs.ADDRESS_OF_NODE_MD)
                    func_def_node.body.append(nodebuilder.rtn(addr_of_instance))
                else:
                    nodeattrs.set_attr(func_def_node, _RECEIVER_TYPE, receiver_ti.name, overwrite=True)

    def _get_new_func_name(self, struct_type_info):
        return "New%s" % struct_type_info.name
