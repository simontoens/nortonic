export PYTHONPATH=src

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
# make test TEST=tests/test_attrresolver.py
test:
	python3 ${TEST}

clean: clean_pyc clean_pycache

clean_pyc:
	find . -type f -name "*.pyc" | xargs rm

clean_pycache:
	find . -type d -name "__pycache__" | xargs rm -r

.PHONY: tests

