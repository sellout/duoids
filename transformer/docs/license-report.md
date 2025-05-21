**NB**: This captures the licenses associated with a particular set of dependency versions. If your own build solves differently, it’s possible that the licenses may have changed, or even that the set of dependencies itself is different. Please make sure you run [`cabal-plan license-report`](https://hackage.haskell.org/package/cabal-plan) on your own components rather than assuming this is authoritative.

# Dependency License Report

Bold-faced **`package-name`**s denote standard libraries bundled with `ghc-9.10.1`.

## Direct dependencies of `transformer-duoids:lib:transformer-duoids`

| Name | Version | [SPDX](https://spdx.org/licenses/) License Id | Description | Also depended upon by |
| --- | --- | --- | --- | --- |
| **`base`** | [`4.20.0.0`](http://hackage.haskell.org/package/base-4.20.0.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/base-4.20.0.0/src/LICENSE) | Core data structures and operations | *(core library)* |
| `duoids` | [`0.0.1.0`](http://hackage.haskell.org/package/duoids-0.0.1.0) |  *MISSING* | *MISSING* |  |
| **`transformers`** | [`0.6.1.1`](http://hackage.haskell.org/package/transformers-0.6.1.1) | [`BSD-3-Clause`](http://hackage.haskell.org/package/transformers-0.6.1.1/src/LICENSE) | Concrete functor and monad transformers |  |

## Indirect transitive dependencies

| Name | Version | [SPDX](https://spdx.org/licenses/) License Id | Description | Depended upon by |
| --- | --- | --- | --- | --- |
| **`ghc-bignum`** | [`1.3`](http://hackage.haskell.org/package/ghc-bignum-1.3) | [`BSD-3-Clause`](http://hackage.haskell.org/package/ghc-bignum-1.3/src/LICENSE) | GHC BigNum library | `ghc-internal` |
| **`ghc-internal`** | [`9.1001.0`](http://hackage.haskell.org/package/ghc-internal-9.1001.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/ghc-internal-9.1001.0/src/LICENSE) | Basic libraries | `base` |
| **`ghc-prim`** | [`0.11.0`](http://hackage.haskell.org/package/ghc-prim-0.11.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/ghc-prim-0.11.0/src/LICENSE) | GHC primitives | *(core library)* |

