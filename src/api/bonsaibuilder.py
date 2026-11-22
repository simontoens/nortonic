"""
Fluent API that assembles small AST parts.
"""
import ast
import lang.nodebuilder as nodebuilder


class BonsaiBuilder:

    def __init__(self):
        # rename
        self._refactor_recepe_decorator_factory = None

    def function(self, name):
        return self._with_node(nodebuilder.call(name))

    def identifier(self, name):
        return self._with_node(nodebuilder.identifier(name))

    def with_node(self, node):
        return self._with_node(node)

    def _with_node(self, node):
        return _with_node(node, self._refactor_recepe_decorator_factory)

    def set_refactor_recepe_decorator_factory(self, decorator_factory):
        """
        decorator_factory is a callable that takes the delegate refactor recipe
        instance and returns the decorator instance.
        """
        self._refactor_recepe_decorator_factory = decorator_factory
        return self


    class Root:

        def __init__(self, node):
            self._node = node

        @property
        def node(self):
            return self._node


    class Continuation(Root):

        def call_method(self, name):
            return _with_node(nodebuilder.attr_call(self._node, name))

        @property
        def node(self):
            return self._node


    class CallContinuation(Continuation):

        def add_argument(self, n):
            self._node.args.append(_unwrap_ast_or_const(n))
            return _with_node(self._node)


    class RefactorRecepe(Root):
        pass


    class IdentifierRefactorRecepe(RefactorRecepe):

        def rename(self, new_name):
            self._node.id = new_name
            return _with_node(self._node)


    class FunctionRefactorRecepe(RefactorRecepe):

        def rename(self, new_name):
            self._node.func.id = new_name
            return _with_node(self._node)

        def refactor_to_method_call(self):
            attr_node = ast.Attribute()
            # by default the first arg becomes the target instance
            # this is necessary for the case str1 == str2, which should be
            # converted to str1.equals(str2)
            attr_node.value = self._node.args[0]
            attr_node.attr = self._node.func.id
            del self._node.args[0]
            self._node.func = attr_node
            return _with_node(self._node)


    class MethodRefactorRecepe(RefactorRecepe):
        
        def rename(self, new_name):
            self._node.func.attr = new_name
            return _with_node(self._node)

        def refactor_to_function_call(self, append_target_instance=False):
            target_node = self._node.func.value
            if append_target_instance:
                self._node.args.append(target_node)
            else:
                self._node.args.insert(0, target_node)
            self._node.func = nodebuilder.identifier(self._node.func.attr)
            return _with_node(self._node)


class _Proxy:

    def __init__(self, delegates):
        self._delegates = delegates

    def __getattr__(self, name):
        for delegate in self._delegates:
            if hasattr(delegate, name):
                return getattr(delegate, name)
        raise AttributeError("%s cannot be used on this bonsai delegates %s " % (name, self._delegates))

        
def _unwrap_ast_or_const(n):
    if isinstance(n, _Proxy):
        n = n._delegates[0].node
    if isinstance(n, BonsaiBuilder.Root):
        n = n.node
    if isinstance(n, ast.AST):
        return n
    return nodebuilder.constant(n)
            
        
def _with_node(node, refactor_recepe_decorator_factory=None):
    continuation = None
    refactor_recepe = None
    if isinstance(node, ast.Name):
        continuation = BonsaiBuilder.Continuation(node)
        refactor_recepe = BonsaiBuilder.IdentifierRefactorRecepe(node)
    if isinstance(node, ast.Call):
        continuation = BonsaiBuilder.CallContinuation(node)
        if isinstance(node.func, ast.Attribute):
            refactor_recepe = BonsaiBuilder.MethodRefactorRecepe(node)
        else:
            refactor_recepe = BonsaiBuilder.FunctionRefactorRecepe(node)
    assert continuation is not None
    org_re = None # trying
    if refactor_recepe_decorator_factory is not None:
        org_re = refactor_recepe # trying
        refactor_recepe = refactor_recepe_decorator_factory(refactor_recepe)
    delelgates = [continuation, refactor_recepe]
    if org_re is not None:
        pass
        #delelgates.append(org_re)
    return _Proxy(delelgates)
