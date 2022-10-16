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
List<String> l = new ArrayList<>(List.of("bye", "world"));
for (String word : l) {
    System.out.println(String.format("%s %s %s %d", "The word", word, "has half as many characters:", word.length() * 2));
}
""")
        self.elisp(py, expected="""
(setq l (list "bye" "world"))
(dolist (word l)
    (message "%s %s %s %s" "The word" word "has half as many characters:" (* (length word) 2)))
""")


if __name__ == '__main__':
    unittest.main()
