import compilertest
import unittest


class FileTest(compilertest.CompilerTest):

    def test_open_with_join(self):
        py = """
import os
root = "a/b/c"
f = open(os.path.join(root, "CODEOWNERS"), "w")
"""
        self.py(py, expected="""
import os

root = "a/b/c"
f = open(os.path.join(root, "CODEOWNERS"), "w")
""")

        self.java(py, expected="""
import java.nio.file.Paths;

static String root = "a/b/c";
static File f = new File(String.valueOf(Paths.get(root, "CODEOWNERS")));
""")

        self.go(py, expected="""
import (
    "path/filepath"
)

root := "a/b/c"
f, _ := os.Create(filepath.Join(root, "CODEOWNERS"))
""")

        self.elisp(py, expected="""
(setq root "a/b/c")
(setq f (f-join root "CODEOWNERS"))
""")

    def test_read(self):
        py = """
f = open("a/b/c")
s = f.read()
"""
        self.py(py, expected=py)
        self.java(py, expected="""
static File f = new File("a/b/c");
static String s = Files.readString(f.toPath());
""")
        self.elisp(py, expected="""
(setq f "a/b/c")
(setq s (with-temp-buffer
    (insert-file-contents f)
    (buffer-string)))
""")
        self.go(py, expected="""
f, _ := os.Open("a/b/c")
t, _ := os.ReadFile(f.Name())
s := string(t)
""")
        

    def test_readlines(self):
        py = """
f = open("a/b/c")
lines = f.readlines()
"""
        self.py(py, expected=py)
        self.java(py, expected="""
import java.util.ArrayList;
import java.util.List;

static File f = new File("a/b/c");
static List<String> lines = Arrays.asList(Files.readString(f.toPath()).split("\\n"));
""")
        self.elisp(py, expected="""
(setq f "a/b/c")
(setq lines (split-string
    (with-temp-buffer
        (insert-file-contents f)
        (buffer-string))
    "\\n"))
""")
        self.go(py, expected="""
f, _ := os.Open("a/b/c")
t, _ := os.ReadFile(f.Name())
lines := strings.Split(string(t), "\\n")
""")

    def test_read__single_stmt(self):
        py = """
print(open("a/b/c").read())
"""
        self.py(py, expected=py)
        self.java(py, expected="""
System.out.println(Files.readString(new File("a/b/c").toPath()));
""")
        self.elisp(py, expected="""
(message (with-temp-buffer
    (insert-file-contents "a/b/c")
    (buffer-string)))
""")
        self.go(py, expected="""
import (
    "fmt"
)

t, _ := os.Open("a/b/c")
t1, _ := os.ReadFile(t.Name())
fmt.Println(string(t1))	
""")

    def test_write(self):
        py = """
f = open("a/b/c", "w")
content = "we are the world"
f.write(content)
"""
        self.py(py, expected=py)
        self.java(py, expected="""
static File f = new File("a/b/c");
static String content = "we are the world";
Files.writeString(f.toPath(), content, Charset.defaultCharset());
""")
        self.elisp(py, expected="""
(setq f "a/b/c")
(setq content "we are the world")
(with-temp-file f
    (insert content))
""")
        self.go(py, expected="""
f, _ := os.Create("a/b/c")
content := "we are the world"
_ = os.WriteFile(f.Name(), []byte(content), 0644)
""")

    def test_write__single_stmt(self):
        py = """
open("a/b/c", "w").write("we are the world")
"""
        self.py(py, expected=py)
        self.java(py, expected="""
Files.writeString(new File("a/b/c").toPath(), "we are the world", Charset.defaultCharset());
""")
        self.elisp(py, expected="""
(with-temp-file "a/b/c"
    (insert "we are the world"))
""")
        self.go(py, expected="""
t, _ := os.Create("a/b/c")
_ = os.WriteFile(t.Name(), []byte("we are the world"), 0644)
""")


if __name__ == '__main__':
    unittest.main()
