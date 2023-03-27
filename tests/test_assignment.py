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

    def test_aug_assign_int(self):
        py = """
a = 1
a += 1
"""
        self.py(py, expected=py)
        self.java(py, expected="""
static Integer a = 1;
a += 1;
""")
        self.elisp(py, expected="""
(setq a 1)
(setq a (+ a 1))
""")
        self.go(py, expected="""
a := 1
a += 1
""")

    def test_aug_assign_string(self):
        py = """
a = "name"
a += "ka"
"""
        self.py(py, expected=py)
        self.java(py, expected="""
static String a = "name";
a += "ka";
""")
        self.elisp(py, expected="""
(setq a "name")
(setq a (concat a "ka"))
""")
        self.go(py, expected="""
a := "name"
a += "ka"
""")

    def test_assign_result_of_comparison__int(self):
        py = "r = 2 == 1"
        self.py(py, expected=py)
        self.java(py, expected="static Boolean r = 2 == 1;")
        self.elisp(py, expected="(setq r (equal 2 1))")
        self.go(py, expected="r := 2 == 1")

    def test_assign_result_of_comparison__string(self):
        py = 'r = "foo" == "blah"'
        self.py(py, expected=py)
        self.java(py, expected='static Boolean r = "foo".equals("blah");')
        self.elisp(py, expected='(setq r (equal "foo" "blah"))')
        self.go(py, expected='r := "foo" == "blah"')

    def test_assign_list(self):
        py = "l = [1,2]"
        self.py(py, expected='l = [1, 2]')
        self.java(py, expected='static List<Integer> l = new ArrayList<>(List.of(1, 2));')
        self.elisp(py, expected='(setq l (list 1 2))')
        self.go(py, expected='l := []int{1, 2}')

    def test_assign_dict(self):
        py = "d={1:2}"
        self.py(py, expected='d = {1: 2}')
        self.java(py, expected='static Map<Integer, Integer> d = new HashMap<>(Map.of(1, 2));')
        self.elisp(py, expected='(setq d #s(hash-table test equal data (1 2)))')
        self.go(py, expected="d := map[int]int{1: 2}")

    def test_assign_tuple_homogeneous_types(self):
        """
        Java: List.of
        """
        py = """t = ("blah", "foo")"""
        self.py(py, expected=py)
        self.java(py, expected='static Tuple<String, String> t = Tuple.of("blah", "foo");')
        self.elisp(py, expected='(setq t (list "blah" "foo"))')
        self.go(py, expected='t := []string{"blah", "foo"}')

    def test_assign_tuple_mixed_types(self):
        """
        Java: the imaginary Tuple type.
        """
        py = "t=(1, 'foo', 1.2)"
        self.py(py, expected='t = (1, "foo", 1.2)')
        self.java(py, expected='static Tuple<Integer, String, Float> t = Tuple.of(1, "foo", 1.2);')
        self.elisp(py, expected='(setq t (list 1 "foo" 1.2))')
        self.go(py, expected='t := []int, string, float32{1, "foo", 1.2}')

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
        self.go(py, expected="""
a := "foo"
a = "blah"
""")
        
    def test_assign_expr(self):
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
        self.go(py, expected="""
a := 1 + 2
fmt.Println(a * 3)
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
        self.go(py, expected="""
var a string
a = "name"
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
        self.go(py, expected="""
t0 := []int{1, 2}
a := t0[0]
b := t0[1]
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
        self.go(py, expected="""
l := []int{1, 2}
a := l[0]
b := l[1]
""")        

    def test_mixed_type_assignment(self):
        py = """
a = 1
a = "foo"
"""
        self.py(py, expected="""
a = 1
a = "foo"
""")

        with self.assertRaises(Exception) as ctx:
            self.java(py, expected="")
        self.assertIn("ident [a] cannot be both a [TypeInfo] <class 'str'> and a [TypeInfo] <class 'int'>", str(ctx.exception))


if __name__ == '__main__':
    unittest.main()
