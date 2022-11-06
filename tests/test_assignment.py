from tests import compilertest
import unittest


class AssignmentTest(compilertest.CompilerTest):

    def test_assign_int(self):
        py = "a = 1"
        self.py(py, expected="a = 1")
        self.java(py, expected="static Integer a = 1;")
        self.elisp(py, expected="(setq a 1)")
        self.go(py, expected="a := 1")

    def test_assign_int2(self):
        py = "a = 1 + 2"
        self.py(py, expected="a = 1 + 2")
        self.java(py, expected="static Integer a = 1 + 2;")
        self.elisp(py, expected="(setq a (+ 1 2))")
        self.go(py, expected="a := 1 + 2")

    def test_assign_float1(self):
        py = "a = 1.2"
        self.py(py, expected="a = 1.2")
        self.java(py, expected="static Float a = 1.2;")
        self.elisp(py, expected="(setq a 1.2)")
        self.go(py, expected="a := 1.2")

    def test_assign_float2(self):
        py = "a = 10 * 1.2"
        self.py(py, expected=" a = 10 * 1.2")
        self.java(py, expected="static Float a = 10 * 1.2;")
        self.elisp(py, expected="(setq a (* 10 1.2))")
        self.go(py, expected="a := 10 * 1.2")

    def test_assign_string(self):
        py = "a = 'name'"
        self.py(py, expected='a = "name" ')
        self.java(py, expected='static String a = "name";')
        self.elisp(py, expected='(setq a "name")')
        self.go(py, expected='a := "name"')

    def test_assign_string_string(self):
        py = "a = 'name' + 'name2'"
        self.py(py, expected='a = "name" + "name2"')
        self.java(py, expected='static String a = "name" + "name2";')
        self.elisp(py, expected='(setq a (concat "name" "name2"))')
        self.go(py, expected='a := "name" + "name2"')

    def test_assign_string_int(self):
        py = "a = 'name' + 1"
        self.py(py, expected='a = "name" + 1') # doesn't work in python actually
        self.java(py, expected='static String a = "name" + 1;')
        self.elisp(py, expected='(setq a (concat "name" (int-to-string 1)))')
        self.go(py, expected='a := "name" + string(1)')

    def test_assign_string_float(self):
        py = "a = 'name' + 1.2" # doesn't work in python actually
        self.py(py, expected='a = "name" + 1.2')
        self.java(py, expected='static String a = "name" + 1.2;')
        self.elisp(py, expected='(setq a (concat "name" (int-to-string 1.2)))')
        self.go(py, expected='a := "name" + string(1.2)')

    def test_assign_result_of_comparison__int(self):
        py = "r = 2 == 1"
        self.py(py, expected=py)
        self.java(py, expected="static Boolean r = 2 == 1;")
        self.elisp(py, expected="(setq r (equal 2 1))")

    def test_assign_result_of_comparison__string(self):
        py = 'r = "foo" == "blah"'
        self.py(py, expected=py)
        self.java(py, expected='static Boolean r = "foo".equals("blah");')
        self.elisp(py, expected='(setq r (equal "foo" "blah"))')

    def test_assign_list(self):
        py = "l = [1,2]"
        self.py(py, expected='l = [1, 2]')
        self.java(py, expected='static List<Integer> l = new ArrayList<>(List.of(1, 2));')
        self.elisp(py, expected='(setq l (list 1 2))')

    def test_assign_dict(self):
        py = "d={1:2}"
        self.py(py, expected='d = {1: 2}')
        self.java(py, expected='static Map<Integer, Integer> d = new HashMap<>(Map.of(1, 2));')
        self.elisp(py, expected='(setq d #s(hash-table test equal data (1 2)))')

    def test_assign_tuple_homogeneous_types(self):
        """
        tuple -> list, this is easier to handle in Java.
        """
        py = "t=('blah', 'foo')"
        self.py(py, expected='t = ["blah", "foo"]')
        self.java(py, expected='static List<String> t = new ArrayList<>(List.of("blah", "foo"));')
        self.elisp(py, expected='(setq t (list "blah" "foo"))')

    def test_assign_tuple_mixed_types(self):
        """
        tuple -> list, but not if we have mixed types.
        """
        py = "t=(1, 'foo', 1.2)"
        self.py(py, expected='t = (1, "foo", 1.2)')
        self.java(py, expected='static Tuple<Integer, String, Float> t = Tuple.of(1, "foo", 1.2);')
        self.elisp(py, expected='(setq t (list 1 "foo" 1.2))')

    def test_assign_ref1(self):
        py = "a = 'hello' ;print(a)"
        self.py(py, expected="""
a = "hello"
print(a)
""")
        self.java(py, expected="""
static String a = "hello";
System.out.println(a);
""")
        self.elisp(py, expected="""
(setq a "hello")
(message a)
""")

    def test_reassign(self):
        py = """
a = "foo"
a = "blah"
"""
        self.py(py, expected=py)

        self.java(py, expected="""
static String a = "foo";
a = "blah";
""")

        self.elisp(py, expected="""
(setq a "foo")
(setq a "blah")
""")        
        
    def test_assign_ref2(self):
        py = "a = 1+2 ;print(a*3)"
        self.py(py, expected="""
a = 1 + 2
print(a * 3)
""")
        self.java(py, expected="""
static Integer a = 1 + 2;
System.out.println(a * 3);
""")

        self.elisp(py, expected="""
(setq a (+ 1 2))
(message "%s" (* a 3))
""")

    def test_assign_none(self):
        py = "a=None;a='name'"
        self.py(py, expected="""
a = None
a = "name"
""")
        self.java(py, expected="""
static String a = null;
a = "name";
""")

        self.elisp(py, expected="""
(setq a nil)
(setq a "name")
""")

    def test_unpacking__literal(self):
        py = 'a, b = [1, 2]'
        self.py(py, expected=py)

        self.java(py, expected="""
static List<Integer> t0 = new ArrayList<>(List.of(1, 2));
static Integer a = t0.get(0);
static Integer b = t0.get(1);
""")

        self.elisp(py, expected="""
(setq t0 (list 1 2))
(setq a (nth 0 t0))
(setq b (nth 1 t0))
""")

    def test_unpacking__ident(self):
        py = """
l = [1, 2]
a, b = l
"""
        self.py(py, expected=py)

        self.java(py, expected="""
static List<Integer> l = new ArrayList<>(List.of(1, 2));
static Integer a = l.get(0);
static Integer b = l.get(1);
""")

        self.elisp(py, expected="""
(setq l (list 1 2))
(setq a (nth 0 l))
(setq b (nth 1 l))
""")


if __name__ == '__main__':
    unittest.main()
