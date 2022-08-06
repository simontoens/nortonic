from run import run
import syntax as sy
import unittest


class FileTest(unittest.TestCase):

    def test_read(self):
        py = """
f = open("a/b/c")
s = f.read()
"""
        self._t(syntax=sy.PythonSyntax(), code=py, expected=py)
        self._t(syntax=sy.JavaSyntax(), code=py, expected="""
File f = new File("a/b/c");
String s = Files.readString(f.toPath());
""")
        self._t(syntax=sy.ElispSyntax(), code=py, expected="""
(setq f "a/b/c")
(setq s (with-temp-buffer
    (insert-file-contents f)
    (buffer-string)))
""")

    def test_readlines(self):
        py = """
f = open("a/b/c")
lines = f.readlines()
"""
        self._t(syntax=sy.PythonSyntax(), code=py, expected=py)
        self._t(syntax=sy.JavaSyntax(), code=py, expected="""
File f = new File("a/b/c");
List<String> lines = Arrays.asList(Files.readString(f.toPath()).split("\\n"));
""")
        self._t(syntax=sy.ElispSyntax(), code=py, expected="""
(setq f "a/b/c")
(setq lines (split-string (with-temp-buffer
    (insert-file-contents f)
    (buffer-string)) "\\n"))
""")

    def test_write(self):
        py = """
f = open("a/b/c", "w")
content = "we are the world"
f.write(content)
"""
        self._t(syntax=sy.PythonSyntax(), code=py, expected=py)
        self._t(syntax=sy.JavaSyntax(), code=py, expected="""
File f = new File("a/b/c");
String content = "we are the world";
Files.writeString(f.toPath(), content, Charset.defaultCharset());
""")
        self._t(syntax=sy.ElispSyntax(), code=py, expected="""
(setq f "a/b/c")
(setq content "we are the world")
(with-temp-file f
    (insert content))
""")

    def test_read__single_stmt(self):
        py = """
print(open("a/b/c").read())
"""
        self._t(syntax=sy.PythonSyntax(), code=py, expected=py)
        self._t(syntax=sy.JavaSyntax(), code=py, expected="""
System.out.println(Files.readString(new File("a/b/c").toPath()));
""")
        self._t(syntax=sy.ElispSyntax(), code=py, expected="""
(message (with-temp-buffer
    (insert-file-contents "a/b/c")
    (buffer-string)))
""")


    def _t(self, code, expected, syntax):
        generated_code = run(code, syntax)

        self.assertEqual(expected.strip(), generated_code)
    

if __name__ == '__main__':
    unittest.main()
