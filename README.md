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

static void greet(String name) {
    System.out.println(String.format("Duke says %s", name));
}
greet("Hello World");
```

```
$ make go << EOF
def greet(name):
    print("Gopher says", name)
greet("Hello World")
EOF

import (
    "fmt"
)

func greet(name string) {
    fmt.Println("Gopher says", name)
}
greet("Hello World")
```

```
$ make elisp << EOF
def greet(name):
    print("GNU says", name)
greet("Hello World")
EOF

(defun greet (name)
    (message "%s %s" "GNU says" name))
(greet "Hello World")
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

We use `make` as a convenience wrapper. It isn't strictly required, the commands in the [Makefile](Makefile) can of course be run directly.


## Testing

### Run all tests
```
make tests
```

### Run a single test module
```
make test test=tests/path/<filename>.py
```


## Linting

Nortinic uses [ruff](https://github.com/astral-sh/ruff). Follow the installation instructions.

Currently, we just use the default ruff linting configuration.

Run it using:
```
make lint
```

