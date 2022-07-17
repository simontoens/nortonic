python:
	python3 run.py --python

java:
	python3 run.py --java

javav:
	python3 run.py --java --verbose

elisp:
	python3 run.py --elisp

test:
	python3 -m unittest discover tests
