from run import run
import syntax as syntaxm
import unittest


class LoopTest(unittest.TestCase):

    def test_for_loop(self):
        py = """
l = ["bye", "world"]
for word in l:
    print("The word", word, "has half as many characters:", len(word) * 2)
"""
        self._t(syntax=syntaxm.PythonSyntax(), code=py, expected=py)
        self._t(syntax=syntaxm.JavaSyntax(), code=py, expected="""
List<String> l = List.of("bye", "world");
for (String word : l) {
    System.out.println(String.format("%s %s %s %d", "The word", word, "has half as many characters:", word.length() * 2));
}
""")
        self._t(syntax=syntaxm.ElispSyntax(), code=py, expected="""
(setq l (list "bye" "world"))
(dolist (word l)
    (message "%s %s %s %s" "The word" word "has half as many characters:" (* (length word) 2)))
""")

    def _t(self, code, expected, syntax):
        generated_code = run(code, syntax)

        self.assertEqual(expected.strip(), generated_code.strip())


if __name__ == '__main__':
    unittest.main()
