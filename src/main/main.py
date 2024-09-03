import argparse
import lang.compiler
import lang.target.elisp
import lang.target.golang
import lang.target.java
import lang.target.python
import sys


def _parse_arguments(args):
    parser = argparse.ArgumentParser(description="Go, Python!")
    parser.add_argument("--go", required=False, action="store_true",
                        help="compile to Golang!")    
    parser.add_argument("--python", required=False, action="store_true",
                        help="compile to Python")
    parser.add_argument("--java", required=False, action="store_true",
                        help="compile to Java")
    parser.add_argument("--elisp", required=False, action="store_true",
                        help="compile to elisp")
    parser.add_argument("--verbose", required=False, action="store_true",
                        help="verbose output")
    return parser.parse_args()


def _get_target_language(args):
    if args.python:
        return lang.target.python.PythonSyntax()
    elif args.java:
        return lang.target.java.JavaSyntax()
    elif args.elisp:
        return lang.target.elisp.ElispSyntax()
    elif args.go:
        return lang.target.golang.GolangSyntax()
    else:
        raise Exception("no target specified")


if __name__ == "__main__":
    args = _parse_arguments(sys.argv)
    target = _get_target_language(args)
    with open("src/main/input.py", "r") as f:
        print(lang.compiler.transcompile(f.read(), target, args.verbose))
