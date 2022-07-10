from run import run
import syntax as sy
import unittest


class AssignmentTest(unittest.TestCase):

    def test_assign_int(self):
        py = "a = 1"
        self._t(syntax=sy.PythonSyntax(), code=py, expected="a = 1")
        self._t(syntax=sy.JavaSyntax(), code=py, expected="Integer a = 1;")
        self._t(syntax=sy.ElispSyntax(), code=py, expected="(setq a 1)")

    def test_assign_int2(self):
        py = "a = 1 + 2"
        self._t(syntax=sy.PythonSyntax(), code=py, expected="a = 1 + 2")
        self._t(syntax=sy.JavaSyntax(), code=py, expected="Integer a = 1 + 2;")
        self._t(syntax=sy.ElispSyntax(), code=py, expected="(setq a (+ 1 2))")

    def test_assign_float1(self):
        py = "a = 1.2"
        self._t(syntax=sy.PythonSyntax(), code=py, expected="a = 1.2")
        self._t(syntax=sy.JavaSyntax(), code=py, expected="Float a = 1.2;")
        self._t(syntax=sy.ElispSyntax(), code=py, expected="(setq a 1.2)")

    def test_assign_float2(self):
        py = "a = 10 * 1.2"
        self._t(syntax=sy.PythonSyntax(), code=py, expected=" a = 10 * 1.2")
        self._t(syntax=sy.JavaSyntax(), code=py, expected="Float a = 10 * 1.2;")
        self._t(syntax=sy.ElispSyntax(), code=py, expected="(setq a (* 10 1.2))")

    def test_assign_string(self):
        py = "a = 'name'"
        self._t(syntax=sy.PythonSyntax(), code=py, expected='a = "name" ')
        self._t(syntax=sy.JavaSyntax(), code=py, expected='String a = "name";')
        self._t(syntax=sy.ElispSyntax(), code=py, expected='(setq a "name")')

    def test_assign_string_string(self):
        py = "a = 'name' + 'name2'"
        self._t(syntax=sy.PythonSyntax(), code=py, expected='a = "name" + "name2"')
        self._t(syntax=sy.JavaSyntax(), code=py, expected='String a = "name" + "name2";')
        self._t(syntax=sy.ElispSyntax(), code=py, expected='(setq a (concat "name" "name2"))')

    def test_assign_string_int(self):
        py = "a = 'name' + 1"
        self._t(syntax=sy.PythonSyntax(), code=py, expected='a = "name" + 1')
        self._t(syntax=sy.JavaSyntax(), code=py, expected='String a = "name" + 1;')
        self._t(syntax=sy.ElispSyntax(), code=py, expected='(setq a (concat "name" (int-to-string 1)))')

    def test_assign_list(self):
        py = "l = [1,2]"
        self._t(syntax=sy.PythonSyntax(), code=py, expected='l = [1, 2]')
        self._t(syntax=sy.JavaSyntax(), code=py, expected='List<Integer> l = new ArrayList<>(List.of(1, 2));')
        self._t(syntax=sy.ElispSyntax(), code=py, expected='(setq l (list 1 2))')

    def test_assign_dict(self):
        py = "d={1:2}"
        self._t(syntax=sy.PythonSyntax(), code=py, expected='d = {1: 2}')
        self._t(syntax=sy.JavaSyntax(), code=py, expected='Map<Integer, Integer> d = new HashMap<>(Map.of(1, 2));')
        self._t(syntax=sy.ElispSyntax(), code=py, expected='(setq d #s(hash-table test equal data (1 2)))')

    def test_assign_tuple_homogeneous_types(self):
        """
        tuple -> list, this is easier to handle in Java.
        """
        py = "t=('blah', 'foo')"
        self._t(syntax=sy.PythonSyntax(), code=py, expected='t = ["blah", "foo"]')
        self._t(syntax=sy.JavaSyntax(), code=py, expected='List<String> t = new ArrayList<>(List.of("blah", "foo"));')
        self._t(syntax=sy.ElispSyntax(), code=py, expected='(setq t (list "blah" "foo"))')

    def test_assign_tuple_mixed_types(self):
        """
        tuple -> list, but not if we have mixed types.
        """
        py = "t=(1, 'foo', 1.2)"
        self._t(syntax=sy.PythonSyntax(), code=py, expected='t = (1, "foo", 1.2)')
        self._t(syntax=sy.JavaSyntax(), code=py, expected='Tuple<Integer, String, Float> t = Tuple.of(1, "foo", 1.2);')
        self._t(syntax=sy.ElispSyntax(), code=py, expected='(setq t (list 1 "foo" 1.2))')

    def test_assign_result_of_comparison(self):
        py = "r = 2 == 1"
        self._t(syntax=sy.PythonSyntax(), code=py, expected=py)
        self._t(syntax=sy.JavaSyntax(), code=py, expected="Boolean r = 2 == 1;")
        self._t(syntax=sy.ElispSyntax(), code=py, expected="(setq r (equal 2 1))")

    def test_assign_ref1(self):
        py = "a = 'hello' ;print(a)"
        self._t(syntax=sy.PythonSyntax(), code=py, expected="""
a = "hello"
print(a)
""")
        self._t(syntax=sy.JavaSyntax(), code=py, expected="""
String a = "hello";
System.out.println(a);
""")
        self._t(syntax=sy.ElispSyntax(), code=py, expected="""
(setq a "hello")
(message a)
""")

    def test_reassign(self):
        py = """
a = "foo"
a = "blah"
"""
        self._t(syntax=sy.PythonSyntax(), code=py, expected=py)

        self._t(syntax=sy.JavaSyntax(), code=py, expected="""
String a = "foo";
a = "blah";
""")

        self._t(syntax=sy.ElispSyntax(), code=py, expected="""
(setq a "foo")
(setq a "blah")
""")        
        
    def test_assign_ref2(self):
        py = "a = 1+2 ;print(a*3)"
        self._t(syntax=sy.PythonSyntax(), code=py, expected="""
a = 1 + 2
print(a * 3)
""")
        self._t(syntax=sy.JavaSyntax(), code=py, expected="""
Integer a = 1 + 2;
System.out.println(a * 3);
""")

        self._t(syntax=sy.ElispSyntax(), code=py, expected="""
(setq a (+ 1 2))
(message "%s" (* a 3))
""")

    def test_assign_none(self):
        py = "a=None;a='name'"
        self._t(syntax=sy.PythonSyntax(), code=py, expected="""
a = None
a = "name"
""")
        self._t(syntax=sy.JavaSyntax(), code=py, expected="""
String a = null;
a = "name";
""")

        self._t(syntax=sy.ElispSyntax(), code=py, expected="""
(setq a nil)
(setq a "name")
""")

    def _t(self, code, expected, syntax):
        generated_code = run(code, syntax)

        self.assertEqual(expected.strip(), generated_code)


if __name__ == '__main__':
    unittest.main()
