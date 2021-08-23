#! /bin/bash

set -e
set -x

export PYTHONPATH=.

python3 test/expressionstest.py
python3 test/builtinfunctest.py
python3 test/iftest.py
python3 test/asstest.py
