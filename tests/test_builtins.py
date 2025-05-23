import compilertest
import unittest


class BuiltInFuncTest(compilertest.CompilerTest):

    def test_print__single_arg_int(self):
        py = "print(1)"
        self.py(py, py)
        self.java(py, "System.out.println(1);")
        self.elisp(py, "(message \"%s\" 1)")
        self.go(py, """
import (
    "fmt"
)

fmt.Println(1)
"""
)

    def test_print__single_arg_str(self):
        py = 'print("hello")'
        self.py(py, py)
        self.java(py, "System.out.println(\"hello\");")
        self.elisp(py, "(message \"hello\")")
        self.go(py, """
import (
    "fmt"
)

fmt.Println(\"hello\")
""")

    def test_print__multiple_args(self):
        py = "print(1, \"foo\", 1.2)"
        self.py(py, py)
        self.java(py, "System.out.println(String.format(\"1 foo 1.2\"));")
        self.elisp(py, "(message \"%s %s %s\" 1 \"foo\" 1.2)")
        self.go(py, """
import (
    "fmt"
)

fmt.Println(1, \"foo\", 1.2)
"""
)

    def test_str(self):
        py = "s = str(10)"
        self.py(py, py)
        self.java(py, "static String s = String.valueOf(10);")
        self.elisp(py, "(setq s (int-to-string 10))")
        self.go(py, "s := string(10)")

    def test_input(self):
        py = """name = input("what's your name? ")"""
        self.py(py, py)
        self.java(py, """
System.out.println("what's your name? ");
static String name = new BufferedReader(new InputStreamReader(System.in)).readLine();
""")
        self.elisp(py, """(setq name (read-string "what's your name? "))""")
        self.go(py, """
import (
    "bufio"
    "fmt"
    "os"
)

fmt.Println("what's your name? ")
name, _ := bufio.NewReader(os.Stdin).ReadString('\\n')
""")

    def test_len__string(self):
        py = 'l = len("four")'
        self.py(py, py)
        self.java(py, 'static Integer l = "four".length();')
        self.elisp(py, '(setq l (length "four"))')
        self.go(py, 'l := len("four")')

    def test_len__list(self):
        py = 'l = len((1, 2, 3))'
        self.py(py, py)
        self.java(py, 'static Integer l = Tuple.of(1, 2, 3).size();')
        self.elisp(py, '(setq l (length (list 1 2 3)))')
        self.go(py, 'l := len([]int{1, 2, 3})')

    def test_startswith_assignment(self):
        py = 'b = "four".startswith("f")'
        self.py(py, py)
        self.java(py, 'static Boolean b = "four".startsWith("f");')
        self.elisp(py, '(setq b (string-prefix-p "f" "four"))')
        self.go(py, """
import (
    "strings"
)

b := strings.HasPrefix("four", "f")
""")

    def test_endswith(self):
        py = 'b = "four".endswith("f")'
        self.py(py, py)
        self.java(py, 'static Boolean b = "four".endsWith("f");')
        self.elisp(py, '(setq b (string-suffix-p "f" "four"))')
        self.go(py, """
import (
    "strings"
)

b := strings.HasSuffix("four", "f")""")

    def test_join(self):
        py = 'print(" ".join(["batteries", "included"]))'
        self.py(py, py)
        self.java(py, 'System.out.println(String.join(" ", new ArrayList<>(List.of("batteries", "included"))));')
        self.elisp(py, '(message (mapconcat \'identity (list "batteries" "included") " "))')
        self.go(py, """
import (
    "fmt"
    "strings"
)

fmt.Println(strings.Join([]string{"batteries", "included"}, " "))
""")

    def test_split(self):
        py = 'l = "batteries included".split(" ")'
        self.py(py, py)
        self.java(py, """
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

static List<String> l = Arrays.asList("batteries included".split(" "));
""")
        self.elisp(py, '(setq l (split-string "batteries included" " "))')
        self.go(py, """
import (
    "strings"
)

l := strings.Split("batteries included", " ")
""")

    def test_split_noargs(self):
        py = 'l = "batteries included".split()'
        self.py(py, py)
        self.java(py, """
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

static List<String> l = Arrays.asList("batteries included".split(" "));
""")
        self.elisp(py, '(setq l (split-string "batteries included"))')
        self.go(py, """
import (
    "strings"
)

l := strings.Split("batteries included", " ")
""")

    def test_split_and_strip_first_element(self):
        py = """
s = "last , first".split(",")[0].strip()
print("|%s|" % s)
"""
        self.py(py, py)
        self.java(py, """
import java.util.Arrays;

static String s = Arrays.asList("last , first".split(",")).get(0).trim();
System.out.println(String.format("|%s|", s));
""")
        self.elisp(py, """
(setq s (string-trim (nth 0 (split-string "last , first" ","))))
(message (format "|%s|" s))
""")
        self.go(py, """
import (
    "fmt"
    "strings"
)

s := strings.TrimSpace(strings.Split("last , first", ",")[0])
fmt.Println(fmt.Sprintf("|%s|", s))
""")

    def test_index(self):
        py = 'i = "batteries included".index("b")'
        self.py(py, py)
        self.java(py, 'static Integer i = "batteries included".indexOf("b");')
        self.elisp(py, """
(setq i (cl-search "b" "batteries included"))
(if (equal i nil)
    (setq i -1))
""")
        self.go(py, """
import (
    "strings"
)

i := strings.Index("batteries included", "b")
""")

    def test_find(self):
        py = 'i = "batteries included".find("b")'
        self.py(py, py)
        self.java(py, 'static Integer i = "batteries included".indexOf("b");')
        self.elisp(py, """
(setq i (cl-search "b" "batteries included"))
(if (equal i nil)
    (setq i -1))
""")
        self.go(py, """
import (
    "strings"
)

i := strings.Index("batteries included", "b")
""")

    def test_enumerate(self):
        """
        Only verifies the return type of enumerate.
        """
        py = """
l1 = [3, 2, 1]
l2 = ["foo"]
l3 = enumerate(l1)
l4 = enumerate(l2)
"""
        self.java(py, """
import java.util.ArrayList;
import java.util.List;

static List<Integer> l1 = new ArrayList<>(List.of(3, 2, 1));
static List<String> l2 = new ArrayList<>(List.of("foo"));
static List<Tuple<Integer, Integer>> l3 = enumerate(l1);
static List<Tuple<Integer, String>> l4 = enumerate(l2);
""")

    def test_chained_method_calls(self):
        py = 'b = " FOO ".lower().strip().startswith("f")'
        self.py(py, py)
        self.java(py, 'static Boolean b = " FOO ".toLowerCase().trim().startsWith("f");')
        self.elisp(py, '(setq b (string-prefix-p "f" (string-trim (downcase " FOO "))))')
        self.go(py, """
import (
    "strings"
)

b := strings.HasPrefix(strings.TrimSpace(strings.ToLower(" FOO ")), "f")
""")

    def test_os_sep(self):
        py = """
import os
s = os.sep
"""
        self.py(py, "s = os.sep")
        self.java(py, 'static String s = System.getProperty("file.separator");')
        self.elisp(py, '(setq s "/")')
        self.go(py, """
import (
    "os"
)

s := string(os.PathSeparator)
""")

    def test_path_sep(self):
        py = """
import os
s = os.path.sep
"""
        self.py(py, "s = os.path.sep")
        self.java(py, """
import java.io.File;

static String s = File.separator;
""")
        self.elisp(py, '(setq s "/")')
        self.go(py, """
import (
    "os"
)

s := string(os.PathSeparator)
""")

    def test_os_path_join(self):
        py = """
import os
s = os.path.join("foo", "blah", "goo")
"""
        self.py(py, """
import os

s = os.path.join("foo", "blah", "goo")
""")
        self.java(py, """
import java.nio.file.Paths;

static String s = String.valueOf(Paths.get("foo", "blah", "goo"));
""")
        self.elisp(py, '(setq s (f-join "foo" "blah" "goo"))')
        self.go(py, """
import (
    "path/filepath"
)

s := filepath.Join("foo", "blah", "goo")
""")


if __name__ == '__main__':
    unittest.main()
