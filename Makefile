go:
	python3 run.py --go
py:
	python3 run.py --python

pyv:
	python3 run.py --python --verbose

java:
	python3 run.py --java

javav:
	python3 run.py --java --verbose

elisp:
	python3 run.py --elisp

elispv:
	python3 run.py --elisp --verbose

test:
	python3 -m unittest discover tests
