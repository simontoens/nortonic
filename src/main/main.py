import argparse
import lang.compiler
import lang.target.elisp
import lang.target.golang
import lang.target.java
import lang.target.python
import os
import sys


def _parse_arguments(args):
    parser = argparse.ArgumentParser(description="The Nortonic Transcompiler")
    parser.add_argument("--go", required=False, action="store_true",
                        help="compile to Golang")
    parser.add_argument("--python", required=False, action="store_true",
                        help="compile to Python")
    parser.add_argument("--java", required=False, action="store_true",
                        help="compile to Java")
    parser.add_argument("--elisp", required=False, action="store_true",
                        help="compile to Elisp")
    parser.add_argument("--sourcefile", required=False, type=str,
                        help="Read Python source from the specified file instead of stdin")
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


def _get_arg_value(name, args):
    """
    Honor cmdline args and env vars.
    """
    v = getattr(args, name, None)
    if v is not None:
        return v    
    for key, value in os.environ.items():
        if key.lower() == name:
            return value
    return None


if __name__ == "__main__":
    args = _parse_arguments(sys.argv)
    srcfile = _get_arg_value("srcfile", args)
    if srcfile is None:
        content = sys.stdin.read()
    else:
        with open(srcfile, "r") as f:
            content = f.read()
    target = _get_target_language(args)
    print(lang.compiler.transcompile(content, target, args.verbose))
