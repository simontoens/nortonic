from tests import compilertest
import unittest


class LoopTest(compilertest.CompilerTest):

    def test_for_loop(self):
        py = """
l = ["bye", "world"]
for word in l:
    print("The word", word, "has half as many characters:", len(word) * 2)
"""
        self.py(py, expected=py)

        self.java(py, expected="""
static List<String> l = new ArrayList<>(List.of("bye", "world"));
for (String word : l) {
    System.out.println(String.format("%s %s %s %d", "The word", word, "has half as many characters:", word.length() * 2));
}
""")
        self.elisp(py, expected="""
(setq l (list "bye" "world"))
(dolist (word l)
    (message "%s %s %s %s" "The word" word "has half as many characters:" (* (length word) 2)))
""")

    def test_for_loop_unpacking(self):
        py = """
lists_of_two_words = [("bye", "world"), ("hello", "world")]
for w1, w2 in lists_of_two_words:
    print(w1, w2)
"""
        self.py(py, expected="""
lists_of_two_words = [["bye", "world"], ["hello", "world"]]
for w1, w2 in lists_of_two_words:
    print(w1, w2)
""")

        self.java(py, expected="""
static List<List<String>> lists_of_two_words = new ArrayList<>(List.of(new ArrayList<>(List.of("bye", "world")), new ArrayList<>(List.of("hello", "world"))));
for (List<String> t0 : lists_of_two_words) {
    String w1 = t0.get(0);
    String w2 = t0.get(1);
    System.out.println(String.format("%s %s", w1, w2));
}
""")

        self.elisp(py, expected="""
(setq lists_of_two_words (list(list "bye" "world") (list "hello" "world")))
(dolist (t0 lists_of_two_words)
    (setq w1 (nth 0 t0))
    (setq w2 (nth 1 t0))
    (message "%s %s" w1 w2))
""")

    def test_for_loop_with_enumerate(self):
        """
        enumerate support isn't implemented correctly, but the return type
        is correct, so testing for that.
        """
        py = """
words = ["yo", "world"]
for i, w in enumerate(words):
    print(i, w)
"""
        self.py(py, expected=py)

        self.java(py, expected="""
static List<String> words = new ArrayList<>(List.of("yo", "world"));
for (Tuple<Integer, String> t0 : enumerate(words)) {
    Integer i = t0.get(0);
    String w = t0.get(1);
    System.out.println(String.format("%d %s", i, w));
}
""")

        self.elisp(py, expected="""
(setq words (list "yo" "world"))
(dolist (t0 (enumerate words))
    (setq i (nth 0 t0))
    (setq w (nth 1 t0))
    (message "%s %s" i w))
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
        self.elisp(py, expected="""
(setq l (list 1 2 3))
(dolist (i l)
    (if (equal i 1)
        break)
    (if (equal i 2)
        continue))
""")


if __name__ == '__main__':
    unittest.main()
