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
    print("GNU says", name)
greet("Hello World")
EOF
```

Read from a file instead of stdin:
```
$ make go srcfile=examples/basics.py
...
```


## Supported Target Languages

Target languages are defined [here](src/lang/target).


## Dependencies

This program has been tested with `Python 3.10.1`. It has no dependencies on 3rd party libraries.

There is a convenience Makefile, so `make` is required for that.


## Testing

`make tests`
