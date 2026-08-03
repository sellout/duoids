# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog 1.1](https://keepachangelog.com/en/1.1.0/),
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [0.1.0.0] - 2026-08-04

### Added

- `Applicative` and `Monad` instances for `Commutative` itself, so it can be
  used with `DerivingVia` to give your own commutative `Monad` a `Duoidal`
  instance, rather than only being usable through `Parallel` and `Sequential`
- `commutativeAp` and `sequentialAp`
- `Normal` duoidal instances for the commutative functors in base: `Complex`,
  `Down`, `Dual`, `Identity`, `Max`, `Maybe`, `Min`, `Monoid.First`,
  `Monoid.Last`, `Monoid.Product`, `Proxy`, `Semigroup.First`, `Semigroup.Last`,
  `Solo`, `Sum`, `->`, `(,,)`, `(,,,)`, and `Const`
- Cabal flag `lint`, which (when disabled, as is the default) allows the
  dependency graph to be pruned, potentially avoiding solver & compilation
  issues.

### Changed

- `Applicative (Parallel (Commutative f))` and `Applicative (Sequential
  (Commutative f))` now require `f` to be a `Monad` rather than merely an
  `Applicative`. `Commutative` was always documented as being for commutative
  `Monad`s, but the instances asked for less than they meant; the new
  `Commutative` instances need the stronger constraint.

### Deprecated

- `sequentialLiftA2` — use `sequentialAp` instead

### Fixed

- `no-recursion` is no longer a dependency by default (thanks to the
  aforementioned `lint` change).

## [0.0.1.0] - 2025-11-17

### Added

- initial release of this package

<!-- NB: The version on the left is the Haskell package version (PVP), the version on the right is the repo (tag) version (SemVer). Their only relationship is that a change of any severity on the left implies a change of at least that severity on the right. -->

[0.1.0.0]: https://github.com/sellout/duoids/compare/v0.1.0...v1.0.0
[0.0.1.0]: https://github.com/sellout/duoids/releases/tag/v0.1.0
