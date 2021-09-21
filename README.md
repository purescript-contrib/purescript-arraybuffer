# arraybuffer

[![CI](https://github.com/purescript-contrib/purescript-arraybuffer/workflows/CI/badge.svg?branch=main)](https://github.com/purescript-contrib/purescript-arraybuffer/actions?query=workflow%3ACI+branch%3Amain)
[![Release](https://img.shields.io/github/release/purescript-contrib/purescript-arraybuffer.svg)](https://github.com/purescript-contrib/purescript-arraybuffer/releases)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-arraybuffer/badge)](https://pursuit.purescript.org/packages/purescript-arraybuffer)
[![Maintainer: jacereda](https://img.shields.io/badge/maintainer-jacereda-teal.svg)](https://github.com/jacereda)
[![Maintainer: jamesdbrock](https://img.shields.io/badge/maintainer-jamesdbrock-teal.svg)](https://github.com/jamesdbrock)


Bindings and implementation for mutable JavaScript `ArrayBuffer`s.

An `ArrayBuffer` is a built-in JavaScript object for storage of a flat continuous
region of memory.

The `Typed` module provides a view into an `ArrayBuffer` for array
access of aligned local-machine-endian types, for in-process flat memory operations.

The `DataView` module provides a view into an `ArrayBuffer` for inter-process
flat memory operations.

* [MDN `ArrayBuffer`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ArrayBuffer)
* [ECMA-262 `ArrayBuffer`](https://tc39.es/ecma262/multipage/structured-data.html#sec-arraybuffer-objects)


## Installation

Install `arraybuffer` with [Spago](https://github.com/purescript/spago):

```sh
spago install arraybuffer
```

## Documentation

`arraybuffer` documentation is stored in a few places:

1. Module documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-arraybuffer).
2. Written documentation is kept in the [docs directory](./docs).
3. Usage examples can be found in [the test suite](./test).

If you get stuck, there are several ways to get help:

- [Open an issue](https://github.com/purescript-contrib/purescript-arraybuffer/issues) if you have encountered a bug or problem.
- Ask general questions on the [PureScript Discourse](https://discourse.purescript.org) forum or the [PureScript Discord](https://purescript.org/chat) chat.

## Contributing

You can contribute to `arraybuffer` in several ways:

1. If you encounter a problem or have a question, please [open an issue](https://github.com/purescript-contrib/purescript-arraybuffer/issues). We'll do our best to work with you to resolve or answer it.

2. If you would like to contribute code, tests, or documentation, please [read the contributor guide](./CONTRIBUTING.md). It's a short, helpful introduction to contributing to this library, including development instructions.

3. If you have written a library, tutorial, guide, or other resource based on this package, please share it on the [PureScript Discourse](https://discourse.purescript.org)! Writing libraries and learning resources are a great way to help this library succeed.

## Usage

### Polyfill

This library relies on runtime implementations of
[`ArrayBuffer`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ArrayBuffer)
and
[`DataView`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView)
([Structured Data](https://tc39.es/ecma262/multipage/structured-data.html#sec-structured-data)),
and
[`TypedArray`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray)
([Indexed Collections](https://tc39.es/ecma262/multipage/indexed-collections.html#sec-indexed-collections)).

If you want to be sure that those implementations are available in your target
runtime environment, you might want to consider using a polyfill such as
[__core-js__ Typed Arrays](https://github.com/zloirock/core-js#ecmascript-typed-arrays).

## Related packages

These are some other packages which provide more `ArrayBuffer` features.

### Reading and Writing

* [__arraybuffer-class__](https://pursuit.purescript.org/packages/purescript-arraybuffer-class)
* [__dynamic-buffers__](https://pursuit.purescript.org/packages/purescript-dynamic-buffers)
* [__parsing-dataview__](https://pursuit.purescript.org/packages/purescript-parsing-dataview)
* [__arraybuffer-builder__](https://pursuit.purescript.org/packages/purescript-arraybuffer-builder)

### Node.js

* [__node-buffer__](https://pursuit.purescript.org/packages/purescript-node-buffer)

### UTF

* [__text-encoding__](https://pursuit.purescript.org/packages/purescript-text-encoding)

### Base64

* [__base64-codec__](https://pursuit.purescript.org/packages/purescript-base64-codec)

## Development

Run the tests with

```sh
spago -x spago-test.dhall test
```
