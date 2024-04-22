"""
These tests are Java specific.
"""

from tests import compilertest
import unittest


class JavaTest(compilertest.CompilerTest):

    def test_functions_have_throws_clause(self):
        py = """
def main(): # in Java, requires "throws IOException" since read() throws also
    read()

def read(): # in Java, requires "throws IOException"
    return open("test").readlines()
"""
        self.java(py, """
static void main() throws IOException {
    read();
}
static List<String> read() throws IOException {
    return Arrays.asList(Files.readString(new File("test").toPath()).split("\\n"));
}
""")


if __name__ == '__main__':
    unittest.main()
