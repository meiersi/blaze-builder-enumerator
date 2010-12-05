Blaze Builder Enumerator
========================

This package integrates the builders from the [`blaze-builder` package][1]
package with the [`enumerator` package][2].  It provides infrastructure and
enumeratees for incrementally executing builders and pass the filled chunks to
a bytestring iteratee.

Where packages like [`attoparsec-enumerator`][3] make it easy to parse
sequences of bytes into data types within enumerators, this package handles the
other direction, converting your data back into sequences of bytes.

[1]: http://hackage.haskell.org/package/blaze-builder
[2]: http://hackage.haskell.org/package/enumerator
[3]: http://hackage.haskell.org/package/attoparsec-enumerator
