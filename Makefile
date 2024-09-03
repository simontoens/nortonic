export PYTHONPATH=src

py:
	python3 src/main/main.py --python

go:
	python3 src/main/main.py --go

java:
	python3 src/main/main.py --java

elisp:
	python3 src/main/main.py --elisp

test:
	python3 -m unittest discover tests

clean: clean_pyc clean_pycache

clean_pyc:
	find . -type f -name "*.pyc" | xargs rm

clean_pycache:
	find . -type d -name "__pycache__" | xargs rm -r

