class TypeDeclarationTemplate:
    """
    $type_name
    $identifier

    Templating to build the lhs of a type declaration statement,
    including the assigment binop ('=' typically), but excluding the rhs.
    """
    def __init__(self, template):
        assert template is not None
        self.template = template

    def render(self, type_name, identifier, owning_scope, node_metadata):
        declaration = self.pre_render__hook(self.template, owning_scope, node_metadata)
        if type_name is not None:
            declaration = declaration.replace("$type", type_name)
        declaration = declaration.replace("$identifier", identifier)
        return self.post_render__hook(declaration, owning_scope, node_metadata)

    def pre_render__hook(self, declaration, owning_scope, node_metadata):
        """
        Hook for subclassing.
        """
        return declaration

    def post_render__hook(self, declaration, owning_scope, node_metadata):
        """
        Hook for subclassing.
        """
        return declaration


class FunctionSignatureTemplate:
    """
    $func_name
    $rtn_type
    $visibility
    $rtn_type
    $args_start $arg_name $arg_type $args_end

    python: def $func_name($args_start$arg_name, $args_end)
    java: $visiblity $rtn_type $func_name($arg_type $arg_name,)
    elisp: (defun $func_name ($arg_name )
    """
    def __init__(self, template_string):
        assert "$func_name" in template_string
        assert "$arg_name" in template_string
        assert "$args_start" in template_string
        assert "$args_end" in template_string        

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

    def render(self, function_name, arguments, rtn_type, visibility, owning_scope):
        """
        function_name: string
        arguments: list of tuples [(name, type_name)] - both strings
        """
        signature = self.signature_beginning.replace("$func_name", function_name)
        signature = signature.replace("$visibility", visibility)
        signature = signature.replace("$rtn_type", "void" if rtn_type == None else rtn_type)
        if len(arguments) > 0:
            for arg_name, arg_type_name in arguments:
                signature += self.arg_template.replace("$arg_name", arg_name)
                signature = signature.replace("$arg_type", "" if arg_type_name is None else arg_type_name)
                signature += self.arg_sep
            signature = signature[:-len(self.arg_sep)]
        signature += self.signature_end
        return self.post_render__hook(signature, owning_scope)

    def post_render__hook(self, signature, owning_scope):
        """
        Hook for subclassing.
        """
        return signature
    
