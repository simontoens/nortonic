set -e

export PYTHONPATH=.
python3 tests/test_assignment.py
python3 tests/test_astrewriter.py
python3 tests/test_block_scope.py
python3 tests/test_builtins.py
python3 tests/test_context.py
python3 tests/test_dict.py
python3 tests/test_file.py
python3 tests/test_func.py
