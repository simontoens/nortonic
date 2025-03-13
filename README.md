# Nortonic

Nortonic is an extensible Python transcompiler: it translates Python into other languages, source code to source code.

This project is currently experimental and in development.


## Examples

```
$ make java << EOF
def greet(name):
    print("Duke says", name)
greet("Hello World")
EOF
```

```
$ make go << EOF
def greet(name):
    print("Gopher says", name)
greet("Hello World")
EOF
```

```
$ make elisp << EOF
def greet(name):
    print("Emacs says", name)
greet("Hello World")
EOF
```

Read from a file instead of stdin:
```
$ make go srcfile=src/main/input.py
...
```


## Supported Target Languages

Java
Golang
Elisp
Python
