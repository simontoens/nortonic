import compilertest
import unittest


class LoopTest(compilertest.CompilerTest):

    def test_for_range_loop(self):
        py = """
for i in range(0, 10):
    print(i)
"""
        self.py(py, expected=py)

        self.java(py, expected="""
for (Integer i = 0; i < 10; i += 1) {
    System.out.println(i);
}
""")

        self.elisp(py, expected="""
(cl-loop for i from 0 to 9 by 1 do
    (message "%s" i))
""")

        self.go(py, expected="""
import (
    "fmt"
)

for i := 0; i < 10; i += 1 {
    fmt.Println(i)
}
""")

    def test_for_range_loop__with_step(self):
        py = """
for i in range(10, 0, -2):
    print(i)
"""
        self.py(py, expected=py)

        self.java(py, expected="""
for (Integer i = 10; i > 0; i += -2) {
    System.out.println(i);
}
""")

        self.elisp(py, expected="""
(cl-loop for i downfrom 10 to 1 by 2 do
    (message "%s" i))
""")

        self.go(py, expected="""
import (
    "fmt"
)

for i := 10; i > 0; i += -2 {
    fmt.Println(i)
}
""")

    def test_foreach_loop__iterate_over_list(self):
        py = """
l = ["bye", "world"]
for word in l:
    print("The word", word, "has half as many characters:", len(word) * 2)
"""
        self.py(py, expected=py)

        self.java(py, expected="""
import java.util.ArrayList;
import java.util.List;

static List<String> l = new ArrayList<>(List.of("bye", "world"));
for (String word : l) {
    System.out.println(String.format("The word %s has half as many characters: %d", word, word.length() * 2));
}
""")
        self.elisp(py, expected="""
(setq l (list "bye" "world"))
(dolist (word l)
    (message "%s %s %s %s" "The word" word "has half as many characters:" (* (length word) 2)))
""")

        self.go(py, expected="""
import (
    "fmt"
)

l := []string{"bye", "world"}
for i := 0; i < len(l); i += 1 {
    word := l[i]
    fmt.Println("The word", word, "has half as many characters:", len(word) * 2)
}""")

    def test_foreach_loop__iterate_over_function_rtn(self):
        py = """
def numbers():
    return 1, 2, 3
for num in numbers():
    print(num)
"""
        self.py(py, expected=py)

        self.java(py, expected="""
static Tuple<Integer, Integer, Integer> numbers() {
    return Tuple.of(1, 2, 3);
}
for (Integer num : numbers()) {
    System.out.println(num);
}
""")
        self.elisp(py, expected="""
(defun numbers ()
    (list 1 2 3))
(dolist (num (numbers))
    (message "%s" num))
""")

        self.go(py, expected="""
import (
    "fmt"
)

func numbers() []int {
    return []int{1, 2, 3}
}
t := numbers()
for i := 0; i < len(t); i += 1 {
    num := t[i]
    fmt.Println(num)
}
""")

    def test_foreach_loop_unpacking(self):
        py = """
lists_of_two_words = [("bye", "world"), ("hello", "world")]
for w1, w2 in lists_of_two_words:
    print(w1, w2)
"""
        self.py(py, expected=py)

        self.java(py, expected="""
import java.util.ArrayList;
import java.util.List;

static List<Tuple<String, String>> lists_of_two_words = new ArrayList<>(List.of(Tuple.of("bye", "world"), Tuple.of("hello", "world")));
for (Tuple<String, String> t : lists_of_two_words) {
    String w1 = t.get(0);
    String w2 = t.get(1);
    System.out.println(String.format("%s %s", w1, w2));
}
""")

        self.elisp(py, expected="""
(setq lists_of_two_words (list(list "bye" "world") (list "hello" "world")))
(dolist (t lists_of_two_words)
    (setq w1 (nth 0 t))
    (setq w2 (nth 1 t))
    (message "%s %s" w1 w2))
""")

        self.go(py, expected="""
import (
    "fmt"
)

lists_of_two_words := [][]string{[]string{"bye", "world"}, []string{"hello", "world"}}
for i := 0; i < len(lists_of_two_words); i += 1 {
    t := lists_of_two_words[i]
    w1 := t[0]
    w2 := t[1]
    fmt.Println(w1, w2)
}
""")

    def test_foreach_loop_with_enumerate(self):
        py = """
words = ["yo", "world"]
for i, w in enumerate(words):
    print(i, w)
"""
        self.py(py, expected=py)

        self.java(py, expected="""
import java.util.ArrayList;
import java.util.List;

static List<String> words = new ArrayList<>(List.of("yo", "world"));
for (Integer i = 0; i < words.size(); i += 1) {
    String w = words.get(i);
    System.out.println(String.format("%d %s", i, w));
}
""")

        self.elisp(py, expected="""
(setq words (list "yo" "world"))
(cl-loop for i from 0 to (- (length words) 1) by 1 do
    (setq w (nth i words))
    (message "%s %s" i w))
""")

        self.go(py, expected="""
import (
    "fmt"
)

words := []string{"yo", "world"}
for i := 0; i < len(words); i += 1 {
    w := words[i]
    fmt.Println(i, w)
}
""")

    def test_continue_and_break(self):
        py = """
l = [1, 2, 3]
for i in l:
    if i == 1:
        break
    if i == 2:
        continue
"""
        self.py(py, expected=py)

        self.java(py, expected="""
import java.util.ArrayList;
import java.util.List;

static List<Integer> l = new ArrayList<>(List.of(1, 2, 3));
for (Integer i : l) {
    if (i == 1) {
        break;
    }
    if (i == 2) {
        continue;
    }
}
""")
        # this is incorrect ...
        self.elisp(py, expected="""
(setq l (list 1 2 3))
(dolist (i l)
    (if (equal i 1)
        break)
    (if (equal i 2)
        continue))
""")

        self.go(py, expected="""
l := []int{1, 2, 3}
for i1 := 0; i1 < len(l); i1 += 1 {
    i := l[i1]
    if i == 1 {
        break
    }
    if i == 2 {
        continue
    }
}
""")


if __name__ == '__main__':
    unittest.main()
