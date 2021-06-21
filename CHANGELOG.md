# v11 2021-06-21

Jorge Acereda has graciously donated this package to __purescript-contrib__.
For version 11, we have refactored this library so that it depends only on
other packages in __purescript-contrib__.

https://github.com/purescript-contrib/governance/issues/40

We have removed the dependencies on these non-__purescript-contrib__ packages:

* https://pursuit.purescript.org/packages/purescript-typelevel
* https://pursuit.purescript.org/packages/purescript-quickcheck-combinators

To upgrade to v11, you might need to do a few simple substitutions
to the type declarations in your own dependent code:

* Replace the type `AProxy` with `Proxy` from the Prelude.
* Remove most of the `Nat` typeclass constraints. https://github.com/purescript-contrib/purescript-arraybuffer/issues/29
* Replace any `BytesPerValue a b` typeclass constraints with `BytesPerType a`.

In v11 of this package, we have also upgraded to PureScript v0.14.

