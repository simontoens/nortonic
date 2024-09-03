class TypeDeclarationTemplate:
    """
    $type_name
    $identifier
    $rhs

    Templating to build a type declaration statement such as: a = 1

    In Java, this would look like: $type $identifier = $rhs
    """
    def __init__(self, template):
        assert template is not None
        self.template = template

    def render(self, type_name, identifier, scope, node_attrs):
        """
        Returns the rendered type declaration without the rhs, and a boolean
        indicating whether the rhs should be processed or not (skipped).
        """
        declaration = self.pre_render__hook(self.template, scope, node_attrs)
        if type_name is not None:
            declaration = declaration.replace("$type", type_name)
        declaration = declaration.replace("$identifier", identifier)
        if  "$rhs" in declaration:
            process_rhs = True
            declaration = declaration.replace("$rhs", "")
        else:
            process_rhs = False
        return self.post_render__hook(declaration, scope, node_attrs), process_rhs

    def pre_render__hook(self, declaration, scope, node_attrs):
        """
        Hook for subclassing.
        """
        return declaration

    def post_render__hook(self, declaration, scope, node_attrs):
        """
        Hook for subclassing.
        """
        return declaration


class FunctionSignatureTemplate:
    """
    $func_name
    $rtn_type[:<no return value placeholder>]
    $visibility
    $args_start $arg_name $arg_type $args_end

    python: def $func_name($args_start$arg_name, $args_end)
    java: $visiblity $rtn_type:void $func_name($arg_type $arg_name,)
    elisp: (defun $func_name ($arg_name )
    golang: func $func_name($arg_type $arg_name,) $rtn_type
    """
    def __init__(self, template_string):
        assert "$arg_name" in template_string
        assert "$args_start" in template_string
        assert "$args_end" in template_string
        # func_name is optional because of anonymous functions

        self.no_rtn_value_placeholder = ""
        rtn_type_with_placeholder = "$rtn_type:"
        rtn_type_start_index = template_string.find(rtn_type_with_placeholder)
        if rtn_type_start_index != -1:
            # parse out the :<placeholder>
            rtn_type_end_index = template_string.find(" ", rtn_type_start_index)
            if rtn_type_end_index == -1:
                rtn_type_end_index = len(template_string) - 1
            start_index = rtn_type_start_index + len(rtn_type_with_placeholder)
            self.no_rtn_value_placeholder = template_string[start_index:rtn_type_end_index]
            template_string = template_string[0:start_index-1] + template_string[rtn_type_end_index:]

        args_start_index = template_string.index("$args_start")
        args_end_index = template_string.index("$args_end")

        arg_name_start_index = template_string.index("$arg_name")
        arg_name_end_index =  arg_name_start_index + len("$arg_name")
        if "$arg_type" in template_string:
            arg_type_start_index = template_string.index("$arg_type")
            arg_type_end_index = arg_type_start_index + len("$arg_type")
        else:
            arg_type_start_index = -1            
            arg_type_end_index = -1
        last_arg_token_start_index = max(arg_type_start_index, arg_name_start_index)
        last_arg_token_end_index = max(arg_type_end_index, arg_name_end_index)
        
        self.arg_template = template_string[args_start_index + len("$args_start"):last_arg_token_end_index]
        self.arg_sep = template_string[last_arg_token_end_index:args_end_index]
        self.signature_beginning = template_string[:args_start_index]
        self.signature_end = template_string[args_end_index + len("$args_end"):]

    def render(self, function_name, arguments, rtn_type, visibility, scope, node_attrs):
        """
        function_name: string
        arguments: list of tuples [(name, type_name)] - both strings
        """
        signature = self.signature_beginning.replace("$func_name", function_name)
        signature = signature.replace("$visibility", visibility)
        if len(arguments) == 0:
            signature = signature.strip()
        else:
            for arg_name, arg_type_name in arguments:
                signature += self.arg_template.replace("$arg_name", arg_name).strip()
                signature = signature.replace("$arg_type", "" if arg_type_name is None else arg_type_name)
                signature += self.arg_sep
            signature = signature[:-len(self.arg_sep)]
        signature += self.signature_end
        signature = signature.replace("$rtn_type", self.no_rtn_value_placeholder if rtn_type is None else rtn_type)
        signature = signature.strip()
        return self.post_render__hook(signature, function_name, arguments, scope, node_attrs).strip()

    def post_render__hook(self, signature, function_name, arguments, scope, node_attrs):
        """
        Hook for subclassing.
        """
        return signature

    def get_function_body_end_delim(self):
        """
        A string to add to the end of the function body.
        """
        return None
