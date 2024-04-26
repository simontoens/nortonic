"""
  i = "foo".index("f")
->
  i = nil if "foo".index("f") == -1 else "foo".index("f")
???

need to generalize (similar to go error handling?)

Given:
"foo".index("f")

pull up and assign to tmp var (go error handling)

t = "foo".index("f")

Replace usage with 't'

Add if stmt below
if t == -1:
    t = nil
"""


s = "foo"
i = "foo".index("f")
