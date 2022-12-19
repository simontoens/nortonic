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
