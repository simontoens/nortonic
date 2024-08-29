go:
	python3 main/main.py --go
py:
	python3 main/main.py --python

pyv:
	python3 main/main.py --python --verbose

java:
	python3 main/main.py --java

javav:
	python3 main/main.py --java --verbose

elisp:
	python3 main/main.py --elisp

elispv:
	python3 main/main.py --elisp --verbose

test:
	python3 -m unittest discover tests

clean_pyc:
	find . -type f -name "*.pyc" | xargs rm

clean_pycache:
	find . -type d -name "__pycache__" | xargs rm -r

clean: clean_pyc clean_pycache
