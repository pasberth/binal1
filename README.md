# Binal

An alternative JavaScript with strongly static type system and s-expressions.

## Building

To build the compiler,
just you type the following command:

```
make
```

Now the compiler installed at
`./dist/build/binal/binal`.

Q. How to build a source code of Binal?

A. For instance, to build `examples/hello.binal`,
just you type the following commands:

```
$BINAL examples/hello.binal
$ESCODEGEN examples/hello.json > examples/hello.js
```

where

```
BINAL=dist/build/binal/binal
ESCODEGEN=./node_modules/escodegen/bin/esgenerate.js
```
