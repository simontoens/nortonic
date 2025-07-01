This document chronicles the addition of list comprehension support to Nortonic.


### Into - May 16th 2025 - Bank Parking lot

Python's list comprehension syntax is probably one of the most tasty kinds of the syntactic candies Python has to offer:

```python
>>> l = [1, 2, 3]
>>> l2 = [i*2 for i in l]
>>> print(l2)
[2, 4, 6]
```

Our initial implementation will simply handle the list comprehension specific ast nodes and add the ability to "echo" them, by compiling back to Phthon. Once that is working, we will worry about how we can actually transpile this syntax to other target languages.


### Inspecting the ast nodes - Evening of May 16th, dining table

```python
>>> print(ast.dump(ast.parse("[i*2 for i in l]"), indent=2))
Module(
  body=[
    Expr(
      value=ListComp(
        elt=BinOp(
          left=Name(id='i', ctx=Load()),
          op=Mult(),
          right=Constant(value=2)),
        generators=[
          comprehension(
            target=Name(id='i', ctx=Store()),
            iter=Name(id='l', ctx=Load()),
            ifs=[],
            is_async=0)]))],
  type_ignores=[])
```

This tells us that we have to worry about node type `ast.ListComp` and the `ast.comprehension` type. The special `_fields` attribute confirms the attribute names we need to care about:

```python
>>> ast.ListComp._fields
('elt', 'generators')
>>> ast.comprehension._fields
('target', 'iter', 'ifs', 'is_async')
```

#### Initial goals and simplifying assumptions

For the initial implementation we will:
- not worry about multiple generators
- ignore the fields `ifs` and `is_async`


#### The implementation

Ok let's try to se what happens when we run a list_comprehension expression through Nortonic:

```
$ make py << EOF
l = [1,2,3]
l2 = [i*2 for i in l]
print(l2)
EOF
```

As expected, we get an error, telling us that we don't support this ast node type yet:

```
TODO
```

So we start by updating the visitor.visitor module. The `_visit` function needs to check for the `ast.ListComp` type and iterate over the generators. In the same module, the `NoopNodeVisitor`, which doubles as a convenience base class and visitor contract (interface), needs to get another `visit` method.

TODO add info on visit order: "generally in the order we emit back out in a depth first traversal"


Once we make these changes and run, we get an error:

```
  File "/Users/stoens/Code/nortonic/src/visitor/visitor.py", line 266, in name
    self._delegate.name(node, num_children_visited)
  File "/Users/stoens/Code/nortonic/src/visitor/visitors.py", line 1084, in name
    assert decl_node is not None, "Unknown identifier [%s]" % node.id
AssertionError: Unknown identifier [i]
```

#### Types and Scopes

Type inference - this is what a large part of Nortonic is about, and this error is related: we are checking here that every ast.Name node has a known declaration node. We need to register to declaration node for the identifier `i` in the expression `[i*2 for i in l]`:
   - `for i in l` here i is declared
   - `i*2` the previously declared i is used

We also need to think about the scope of i - does a list comprehension have its own scope?

We can check:
```python
>>> i = 101
>>> l = [1]
>>> l2 = [i for i in range(0, 10)]
>>> print(i)
101
```

Contrast with:
```python
>>> i = 101
>>> for i in range(0, 10):
...      pass
... 
>>> print(i)
9
```

Given the above, we need a new scope for list comprehension only. This is important so we can track declaration nodes properly. This requires adding a new method to visitor.scopedecorator.py, to the class ScopeDecorator; which extends NoopNodeVisitor, where we added new visitor methods above.

The changes up to this point are in this [commit](efa9445af9dbb889bab44055295814193dd1d071).
-


The next error is:
```
    return self._delegate.should_revisit
  File "/Users/stoens/Code/nortonic/src/visitor/typevisitor.py", line 36, in should_revisit
    assert self.num_visits < 6, "cannot resolve all references, check method _assert_resolved_type"
AssertionError: cannot resolve all references, check method _assert_resolved_type
```

This is the most common Nortonic error you will see, and it has to do with our type inference logic not being able to determine the Python type of all ast nodes.


### Type Visitor March 17th - Subway

The TypeVisitor class assigns type information (TypeInfo instances) to each ast node. For the list comprehension support we are adding, we need to determine the type of `l2`:
```
l2 = [i*2 for i in l]
```
To get there, we need to make sure that the ast.Name nodes for `i` have the right type. Finally we have to set type of entire expression evaluates to on the ast.ListComp node.

As above, we need to implement the new visitor methods in the [TypeVisitor]() class.

Strategy:

`for i in l` -> `l` an iterable over some contained type `l[x]`. Both types `l` and `x` have been determined already by the TypeVisitor. We can look at how a regular for loop is implemented to see how we get from `l[x]` to `x` and it turns out, after inspection, that `TypeVisitor.loop_for` does seemingly exactly what we need already.  For clarity, we will first copy/paste and then refactor.

The [initial change](86afe1645568d6adbc97e132a32e111511ffaa28) adds the `list_comp_generator` visitor method with the logic described above. It also adds the `list_comp` visitor method, which just takes the type of ast.ListComp.elt (see [ast representation above]()) and promotes it to a list, because the return type of a list comprehension expression is always a list.

Since the logic in `list_comp_generator` is the same as in `loop_for`, we can extract and [share it](aa14448a3da67700cee95fbeeb5ab5f1511b9f44). We run all tests to make sure no regressions were introduced:

```
$ make tests
Ran 225 tests in 10.264s
OK
```


Re-running Nortonic, we now do not get an error anymore, but the output does not look right:

```
$ make py << EOF
l = [1,2,3]
l2 = [i*2 for i in l]
print(l2)
EOF

python3 src/main/main.py --python
l = [1, 2, 3]
l2 = i l i * 2
print(l2)
```

Specifically the list comprehension line is wrong: `i l i * 2`. The next task is to update the [TokenVisitor](src/visitor/tokenvisitor.py] class, which is responsible of traversing the ast and emitting character groups.



### Token Visitor - March 18th - Soccer training


### Adding tests

