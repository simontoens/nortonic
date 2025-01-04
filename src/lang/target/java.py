import ast
import functools
import lang.internal.typeinfo as ti
import lang.target.rewrite as rewrite
import lang.target.targetlanguage as targetlanguage
import lang.target.templates as templates
import os
import visitor.asttoken as asttoken
import visitor.nodeattrs as nodeattrs
import visitor.visitor as visitor


THROWS_EXCEPTION = "java__throws"


class JavaFunctionSignatureTemplate(templates.FunctionSignatureTemplate):

    def __init__(self):
        super().__init__("$rtn_type:void $func_name($args_start$arg_type $arg_name, $args_end) $throws")

    def post_render__hook(self, signature, function_name, arguments, scope, node_attrs):
        class_name, _ = scope.get_enclosing_class()
        if class_name is None:
            # this static logic needs to be redone, we really just want the
            # main method to be static a this point, until there's support
            # for @classmethod
            signature = "static " + signature
        else:
            signature = "public " + signature

        throws = "throws %s" % node_attrs[THROWS_EXCEPTION] if THROWS_EXCEPTION in node_attrs else ""
        signature = signature.replace("$throws", throws)
        return signature

    def pre_render__hook(self, function_name, rtn_type, scope, node_attrs):
        func = node_attrs[nodeattrs.FUNC_NODE_ATTR]
        if func.is_constructor:
            # __init__ -> class name
            function_name = func.get_rtn_type_info().name
            # class Foo's __init__ -> public Foo(...)
            rtn_type = ""
        return function_name, rtn_type


class JavaTypeDeclarationTemplate(templates.TypeDeclarationTemplate):

    def __init__(self):
        super().__init__("$type $identifier = $rhs")

    def post_render__hook(self, declaration, scope, node_attrs):
        if scope.is_class:
            declaration = "public " + declaration
        elif not scope.has_parent:
            declaration = "static " + declaration
        return declaration


class JavaSyntax(targetlanguage.AbstractTargetLanguage):

    def __init__(self):
        super().__init__(formatter=JavaFormatter(),
                         is_prefix=False,
                         stmt_end_delim=";", stmt_end_delim_always_required=True,
                         block_start_delim="{", block_end_delim="}",
                         flow_control_test_start_delim="(", flow_control_test_end_delim=")",                    
                         eq_binop="==", # rewritten to equals for obj comparison
                         loop_foreach_keyword=":",
                         arg_delim=",",
                         object_instantiation_op="new",
                         has_block_scope=True,
                         has_assignment_lhs_unpacking=False,
                         ternary_replaces_if_expr=True,
                         type_declaration_template=JavaTypeDeclarationTemplate(),
                         function_signature_template=JavaFunctionSignatureTemplate())

        self.type_mapper.register_none_type_name("null")
        self.type_mapper.register_simple_type_mapping(int, "Integer")
        self.type_mapper.register_simple_type_mapping(float, "Float")
        self.type_mapper.register_simple_type_mapping(str, "String")
        self.type_mapper.register_simple_type_mapping(bool,"Boolean", lambda v: "true" if v else "false")

        self.type_mapper.register_container_type_mapping(
            list,
            "List<$contained_type$[0]>",
            start_literal="new ArrayList<>(",
            end_literal=")",
            start_values_wrapper="List.of(",
            end_values_wrapper=")",
            homogenous_types=True,
            imports=["java.util.%s" % c for c in ("List", "ArrayList")])


        # self.type_mapper.register_container_type_mapping(
        #     tuple,
        #     "List<$contained_type$[0]>",
        #     start_literal="new ArrayList<>(",
        #     end_literal=")",
        #     start_values_wrapper="List.of(",
        #     end_values_wrapper=")"),
        self.type_mapper.register_container_type_mapping(
            tuple,
            "Tuple<$contained_type$[]>",
            start_literal="Tuple.of(",
            end_literal=")")


        # rethink this - somtimes Tuple carries meaning, such as when it is
        # returned from a function to wrap multiple return values
        # ...
        # homogeneous tuple -> translate it to a read-only list
        # self.type_mapper.register_container_type_mapping(
        #     tuple,
        #     "List<$contained_type>${1}",
        #     start_literal="List.of(",
        #     end_literal=")",
        #     apply_if=lambda type_info: type_info.contains_homogeneous_types)
        
        # non-homogeneous list -> translate it to a Tuple
        self.type_mapper.register_container_type_mapping(
            list,
            "Tuple<$contained_type$[]>",
            start_literal="Tuple.of(",
            end_literal=")",
            homogenous_types=False)
        
        self.type_mapper.register_container_type_mapping(
            dict,
            "Map<$contained_type$[0], $contained_type$[1]>",
            start_literal="new HashMap<>(Map.of(",
            end_literal="))",
            values_separator=",")

        print_fmt = {int: "%d", float: "%d", str: "%s"}
        self.register_rewrite(print, rename_to="System.out.println",
            rewrite=lambda args, rw:
                rw.replace_args_with(
                  rw.call("String.format", rtn_type=str)
                    .prepend_arg(" ".join([print_fmt.get(a.type, "%s") for a in args]))
                      .append_args(args))
                if len(args) > 1 else None)

        self.register_rewrite(rewrite.Keyword.FOR, rewrite=lambda args, rw:
                rw.rewrite_as_c_style_loop()
                    if rw.is_range_loop() or rw.is_enumerated_loop() else None)

        self.register_rewrite(input, arg_type=str,
            rename_to="new BufferedReader(new InputStreamReader(System.in)).readLine",
            rewrite=lambda args, rw:
                rw.insert_above(rw.call(print).append_arg(args[0]))
                  .remove_args())

        self.register_rewrite(len, arg_type=str, rename_to="length",
            rewrite=lambda args, rw: rw.rewrite_as_attr_method_call())

        self.register_rewrite(len, arg_type=(list, tuple), rename_to="size",
            rewrite=lambda args, rw: rw.rewrite_as_attr_method_call())
        
        self.register_rename(str, to="String.valueOf")

        def _rewrite_str_mod(args, rw):
            format_call = rw.call("String.format")
            keep_args = True
            rhs = args[1]
            if rhs.type is tuple:
                keep_args = False
                format_call.append_arg(args[0])
                format_call.append_args(rhs.node.elts)
            rw.replace_node_with(format_call, keep_args)
        self.register_rewrite(rewrite.Operator.MOD, arg_type=str,
                              rewrite=_rewrite_str_mod)

        def _equality_rewrite(args, rw, check_is_equal):
            if args[0].type in (str,):
                f = rw.call("equals")
                rw.replace_node_with(f).rewrite_as_attr_method_call()
                if not check_is_equal:
                    rw.negate()
        self.register_rewrite(rewrite.Operator.EQUALS,
            rewrite=functools.partial(_equality_rewrite, check_is_equal=True))
        self.register_rewrite(rewrite.Operator.NOT_EQUALS,
            rewrite=functools.partial(_equality_rewrite, check_is_equal=False))

        # str
        self.register_rename(str.startswith, to="startsWith")
        self.register_rename(str.endswith, to="endsWith")
        self.register_rename(str.strip, to="trim")
        self.register_rename(str.upper, to="toUpperCase")
        self.register_rename(str.lower, to="toLowerCase")
        self.register_rename(str.index, to="indexOf")
        self.register_rename(str.find, to="indexOf")

        self.register_rewrite(str.join, rename_to="String.join",
            rewrite=lambda args, rw: rw.rewrite_as_func_call(inst_1st=True))

        def _split_rewrite(args, rw):
            if len(args) == 0:
                # python has split(), which splits on whitespace
                rw.append_arg(" ")
            rw.replace_node_with(rw.call("Arrays.asList"),
                                 current_node_becomes_singleton_arg=True)
        self.register_rewrite(str.split, imports="java.util.Arrays",
            rewrite=_split_rewrite)

        def _slice_rewrite(args, rw):
            if len(args) == 2 and isinstance(args[1].node, ast.UnaryOp):
                lhs = rw.call(len).append_arg(rw.target_node)
                rhs = args[1].node.operand
                binop = rw.binop("-", lhs, rhs)
                rw.call_on_target("substring", keep_args=False).append_arg(args[0]).append_arg(binop)
            else:
                rw.call_on_target("substring")
        self.register_rewrite(rewrite.Operator.SUBSCRIPT, arg_type=str,
            rewrite=_slice_rewrite)

        # file
        self.type_mapper.register_simple_type_mapping(ti.TypeInfo.textiowraper(), "File")
        self.register_rewrite(open, arg_type=str, rename_to="new File",
            rewrite=lambda args, rw: rw.keep_first_arg())

        def _read_rewrite(args, rw, is_readlines):
            rw.rewrite_as_func_call().rename("Files.readString")
            file_arg = rw.wrap(rw.arg_nodes[0])
            rw.replace_args_with(file_arg.chain_method_call("toPath"))
            rw.set_node_attr(THROWS_EXCEPTION, "IOException")
            if is_readlines:
                # in python readlines returns a list of strings
                # so we'll call split("\n")
                rw.chain_method_call("split").append_arg("\\n")
                # split returns an Array, so we wrap the whole thing in
                # Arrays.asList
                rw.replace_node_with(rw.call("Arrays.asList"),
                                     current_node_becomes_singleton_arg=True)

        self.register_rewrite("read", inst_type=ti.TypeInfo.textiowraper(),
            rewrite=functools.partial(_read_rewrite, is_readlines=False))

        self.register_rewrite("readlines", inst_type=ti.TypeInfo.textiowraper(),
            rewrite=functools.partial(_read_rewrite, is_readlines=True))

        def _write_rewrite(args, rw):
            rw.rewrite_as_func_call().rename("Files.writeString")
            content_arg = rw.arg_nodes[0]
            file_arg = rw.wrap(rw.arg_nodes[1])
            rw.replace_args_with(
                file_arg.chain_method_call("toPath"))\
                .append_arg(content_arg)\
                .append_arg(rw.xcall("Charset.defaultCharset"))
        self.register_rewrite("write", inst_type=ti.TypeInfo.textiowraper(),
            rewrite=_write_rewrite)


        # list
        self.register_rename(list.append, to="add")

        self.register_rewrite(list.sort,
            rewrite=lambda args, rw: rw.append_arg(rw.const(None)))

        self.register_rewrite(rewrite.Operator.SUBSCRIPT,
            arg_type=(list, tuple, dict),
            rewrite=lambda args, rw: rw.call_on_target("get"))

        self.register_rewrite(rewrite.Operator.DICT_ASSIGNMENT,
            inst_type=dict, rewrite=lambda args, rw: rw.call_on_target("put"))


        # os

        # os.sep is the same as os.path.sep but Java is also a respectable
        # language with different ways of getting at the path sep
        self.register_attr_rewrite("sep", os, rewrite=lambda args, rw:
            rw.replace_node_with(rw.call("System.getProperty")
                .append_arg("file.separator")))

        # os.path

        # os.path.sep is the same as os.sep but Java is also a respectable
        # language with different ways of getting at the path sep
        self.register_attr_rewrite("sep", os.path, imports="java.io.File",
            rewrite=lambda args, rw:
                rw.replace_node_with(rw.ident("File.separator")))

        self.register_rewrite(os.path.join, imports="java.nio.file.Paths",
            rewrite=lambda args, rw:
                rw.replace_node_with(
                    rw.call(str).append_arg(
                        rw.call("Paths.get", rtn_type=ti.TypeInfo.notype)
                            .append_args(args)),
                    keep_args=False))


        self.register_node_visitor(ThrowsVisitor())


class JavaFormatter(targetlanguage.CommonInfixFormatter):

    def requires_space_sep(self, token, remaining_tokens):
        if asttoken.is_boundary_ending_before_value_token(
                remaining_tokens, asttoken.FLOW_CONTROL_TEST):
            # we want if (1 == 1), not if (1 == 1 )
            return False
        return super().requires_space_sep(token, remaining_tokens)


class ThrowsVisitor(visitor.NoopNodeVisitor):
    """
    This visitors adds metadata to FuncDef nodes to indicate they need
    to throw the specified exception.

    TODO this needs to be able to handle multiple exceptions.
    """
    def __init__(self):
        super().__init__()
        self.context = None
        self.func_to_funcdef_node = {}
        self.added_more_md = False

    @property
    def should_revisit(self):
        if self.added_more_md:
            self.added_more_md = False
            return True
        return False

    def call(self, node, num_children_visited):
        super().call(node, num_children_visited)
        if num_children_visited == -1:
            this_call_throws = False
            exception_md = None
            func = nodeattrs.get_function(node)
            if func in self.func_to_funcdef_node:
                this_call_throws = True
                n = self.func_to_funcdef_node[func]
                exception_md = nodeattrs.get_attr(n, THROWS_EXCEPTION)
            elif nodeattrs.has_attr(node, THROWS_EXCEPTION):
                # set by rewrite rule
                this_call_throws = True
                exception_md = nodeattrs.get_attr(node, THROWS_EXCEPTION)
            if this_call_throws:
                scope = self.context.current_scope.get()
                _, ns_node = scope.get_enclosing_namespace()
                if isinstance(ns_node, ast.FunctionDef):
                    if not nodeattrs.has_attr(ns_node, THROWS_EXCEPTION):
                        nodeattrs.set_attr(ns_node, THROWS_EXCEPTION, exception_md)
                        func = nodeattrs.get_function(ns_node)
                        self.func_to_funcdef_node[func] = ns_node
                        self.added_more_md = True
