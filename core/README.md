# Duoids

[![Hackage Version](https://img.shields.io/hackage/v/duoids)](https://hackage.haskell.org/package/duoids)
[![Packaging status](https://repology.org/badge/tiny-repos/haskell:duoids.svg)](https://repology.org/project/haskell:duoids/versions)
[![latest packaged versions](https://repology.org/badge/latest-versions/haskell:duoids.svg)](https://repology.org/project/haskell:duoids/versions)

Unifying parallel and sequential operations

Duoids relate a pair of monoids or applicative functors, where one can be seen as “parallel” and the other “sequential”.

## usage

**NB**: This package currently supports GHC 9.6 and newer. There is no known impediment to supporting older versions. If you need support for an older version, please [open an issue](https://github.com/sellout/duoids/issues). Even if it already compiles on that version, knowing that someone is using it will allow me to add it to my support matrix so it’s likely to continue working going forward.

- [comparisons to similar projects](#comparisons)

### licensing

This package is licensed under [The GNU AGPL 3.0 only, with some alternatives](./LICENSE). If you need a license for usage that isn’t covered under the AGPL, please contact [Greg Pfeil](mailto:greg@technomadic.org?subject=licensing%20duoids).

You should review the [license report](docs/license-report.md) for details about dependency licenses.

### versioning

This project largely follows a strict variation of the [Haskell Package Versioning Policy](https://pvp.haskell.org/) (PVP). See [the specifics](https://sellout.github.io/flaky/haskell/strict-PVP.html).

## contributing

- [contributor guide](https://sellout.github.io/flaky/CONTRIBUTING/haskell.html)

## comparisons

Other projects similar to this one, and how they differ.

### [async](https://hackage.haskell.org/package/async)

The `async` package has a `Concurrently` newtype over `IO` that’s the same as the `Parallel IO` instance here. (In fact, the implementation here is copied from there, with attribution.) However, it doesn’t have anything like the `Duoidal` class, and so requires explicit conversion between `IO` and `Concurrently`.

### [either](https://hackage.haskell.org/package/either)

The `either` package has a `Validation` type that’s isomorphic to `Either` and has an `Applicative` instance that’s the same as the `Parallel (Either e)` instance here. However, it doesn’t have anything like the `Duoidal` class, and so requires manual conversion back and forth between `Either` and `Validation`.

### [Haskerwaul](https://github.com/sellout/haskerwaul#readme)

A broader package that contains a more general (category polymorphic) implementation of duoids. It’s also much less pragmatic.

### [multi-except](https://hackage.haskell.org/package/multi-except)

This seems like another approach to `Validation`, but I haven’t used it.

### [United Monoids](https://github.com/snowleopard/united)

Andrey Mokhov’s independently-discovered implementation. I think united monoids are normal duoids in **Set**. United Monoids also offers a `Semilattice` class and avoids newtypes by treating the parallel semigroup (`overlay` in United Monoids) as primary, with sequential (`connect`) as an additional operation. It also forces both identities to be the same.

In [his related blog post](https://blogs.ncl.ac.uk/andreymokhov/united-monoids/), Andrey talks about united semigroups, but I don’t think they exist in the package. They don’t exist here, because I haven’t seen any research on them (but they do seem useful).
