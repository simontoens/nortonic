# Python source transcompiler

This program translates Python 3.x code into other popular languages.


## Approach

The AST is the source of truth - whenever possible, the AST is modified and re-processed instead of maintaing external metadata.

### Hardcoded metadata

#### Outputs


### TypeVisitor

#### Inputs

#### Outputs

TypeInfo instances are derived from:
- literals and expression
- function return types

TypeInfo instances are cleared before every TypeVisitor invocation. This is more inefficient, but makes reasoning about type inferrence easier.

### AST nodes

A TypeInfo instance is associated with each node

#### Call nodes

The TypeInfo instance associated with a call node is the type of the method or function return value. For user defined functions (in the AST), the return type can be inferred. For builtin functions, the return is defined initially on the Python specific Function instance. Once the Function instance has been associated with a call node, the return TypeInfo is set on the call node instance as node metadata. This is necessary so that the return TypeInfo is not lost as the call node is being rewritten.

### Function metadata

Some function metadata cannot be rebuilt from scratch and needs to be preserved
accross visitor invocations:
- argument types and return types if pointers are involved.
- whether the function populates a container type

