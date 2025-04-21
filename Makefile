export PYTHONPATH=src

help:
	python3 src/main/main.py --help

py:
	python3 src/main/main.py --python

go:
	python3 src/main/main.py --go

java:
	python3 src/main/main.py --java

elisp:
	python3 src/main/main.py --elisp

tests:
	python3 -m unittest discover tests

# runs single test class, for example:
# make test test=tests/test_attrresolver.py
test:
	python3 ${test}

# making the project ruff-compliant is in progress...
lint:
	ruff check src/lang/target

clean: clean_pyc clean_pycache

clean_pyc:
	find . -type f -name "*.pyc" | xargs rm

clean_pycache:
	find . -type d -name "__pycache__" | xargs rm -r

.PHONY: tests

