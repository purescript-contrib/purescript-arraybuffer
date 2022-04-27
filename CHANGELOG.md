# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

Breaking changes:

New features:

Bugfixes:

Other improvements:

## [v13.0.0](https://github.com/purescript-contrib/purescript-arraybuffer/releases/tag/v13.0.0) - 2022-04-27

Breaking Changes:
- Migrate FFI to ES modules (#41 by @JordanMartinez)
- Replaced polymorphic proxies with monomorphic `Proxy` (#41 by @JordanMartinez)

## v12.0.0

Delete the `TypedArray` polyfill which was preventing this
library from working with `purs bundle` v0.14.4.
https://github.com/purescript-contrib/purescript-arraybuffer/issues/34

### Breaking Changes

May lose partial polyfill `TypedArray` support for only the methods present
in regular JavaScript Arrays.
https://web.archive.org/web/20171019084331/https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray#Methods_Polyfill

## v11.0.3

Revert to v11.0.1.
https://github.com/purescript-contrib/purescript-arraybuffer/issues/37

## v11.0.2

Delete the `TypedArray` polyfill which was preventing this
library from working with `purs bundle` v0.14.4.
https://github.com/purescript-contrib/purescript-arraybuffer/issues/34

## v11.0.1

Regenerate `bower.json`.

## v11.0.0

Jorge Acereda has graciously donated this package to __purescript-contrib__.

For version 11, we have refactored this library so that it depends only on
other packages in __purescript-contrib__.

https://github.com/purescript-contrib/governance/issues/40

We have removed the dependencies on these non-__purescript-contrib__ packages:

* https://pursuit.purescript.org/packages/purescript-typelevel
* https://pursuit.purescript.org/packages/purescript-quickcheck-combinators

In v11.0.0 of this package, we have also upgraded to PureScript v0.14.

### Breaking Changes

To upgrade to v11.0.0, you might need to do a few substitutions
to the type declarations in your own dependent code:

* Replace the type `AProxy` with `Proxy` from the Prelude.
* Remove most of the `Nat` typeclass constraints. https://github.com/purescript-contrib/purescript-arraybuffer/issues/29
* Replace any `BytesPerValue a b` typeclass constraints with `BytesPerType a`.

We have also privatized `Typed.part'`. https://github.com/purescript-contrib/purescript-arraybuffer/issues/32

