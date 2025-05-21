**NB**: This captures the licenses associated with a particular set of dependency versions. If your own build solves differently, it’s possible that the licenses may have changed, or even that the set of dependencies itself is different. Please make sure you run [`cabal-plan license-report`](https://hackage.haskell.org/package/cabal-plan) on your own components rather than assuming this is authoritative.

# Dependency License Report

Bold-faced **`package-name`**s denote standard libraries bundled with `ghc-9.10.1`.

## Direct dependencies of `algebraic-graph-duoids:lib:algebraic-graph-duoids`

| Name | Version | [SPDX](https://spdx.org/licenses/) License Id | Description | Also depended upon by |
| --- | --- | --- | --- | --- |
| `algebraic-graphs` | [`0.7`](http://hackage.haskell.org/package/algebraic-graphs-0.7) | [`MIT`](http://hackage.haskell.org/package/algebraic-graphs-0.7/src/LICENSE) | A library for algebraic graph construction and transformation |  |
| **`base`** | [`4.20.0.0`](http://hackage.haskell.org/package/base-4.20.0.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/base-4.20.0.0/src/LICENSE) | Core data structures and operations | *(core library)* |
| `duoids` | [`0.0.1.0`](http://hackage.haskell.org/package/duoids-0.0.1.0) |  *MISSING* | *MISSING* |  |

## Indirect transitive dependencies

| Name | Version | [SPDX](https://spdx.org/licenses/) License Id | Description | Depended upon by |
| --- | --- | --- | --- | --- |
| **`array`** | [`0.5.7.0`](http://hackage.haskell.org/package/array-0.5.7.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/array-0.5.7.0/src/LICENSE) | Mutable and immutable arrays | `algebraic-graphs`, `containers`, `deepseq` |
| **`containers`** | [`0.7`](http://hackage.haskell.org/package/containers-0.7) | [`BSD-3-Clause`](http://hackage.haskell.org/package/containers-0.7/src/LICENSE) | Assorted concrete container types | `algebraic-graphs` |
| **`deepseq`** | [`1.5.0.0`](http://hackage.haskell.org/package/deepseq-1.5.0.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/deepseq-1.5.0.0/src/LICENSE) | Deep evaluation of data structures | `algebraic-graphs`, `containers`, `pretty` |
| **`ghc-bignum`** | [`1.3`](http://hackage.haskell.org/package/ghc-bignum-1.3) | [`BSD-3-Clause`](http://hackage.haskell.org/package/ghc-bignum-1.3/src/LICENSE) | GHC BigNum library | `ghc-internal` |
| **`ghc-boot-th`** | [`9.10.1`](http://hackage.haskell.org/package/ghc-boot-th-9.10.1) | [`BSD-3-Clause`](http://hackage.haskell.org/package/ghc-boot-th-9.10.1/src/LICENSE) | Shared functionality between GHC and the @template-haskell@ library | `template-haskell` |
| **`ghc-internal`** | [`9.1001.0`](http://hackage.haskell.org/package/ghc-internal-9.1001.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/ghc-internal-9.1001.0/src/LICENSE) | Basic libraries | `base` |
| **`ghc-prim`** | [`0.11.0`](http://hackage.haskell.org/package/ghc-prim-0.11.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/ghc-prim-0.11.0/src/LICENSE) | GHC primitives | *(core library)* |
| **`pretty`** | [`1.1.3.6`](http://hackage.haskell.org/package/pretty-1.1.3.6) | [`BSD-3-Clause`](http://hackage.haskell.org/package/pretty-1.1.3.6/src/LICENSE) | Pretty-printing library | `template-haskell` |
| **`template-haskell`** | [`2.22.0.0`](http://hackage.haskell.org/package/template-haskell-2.22.0.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/template-haskell-2.22.0.0/src/LICENSE) | Support library for Template Haskell | `containers` |
| **`transformers`** | [`0.6.1.1`](http://hackage.haskell.org/package/transformers-0.6.1.1) | [`BSD-3-Clause`](http://hackage.haskell.org/package/transformers-0.6.1.1/src/LICENSE) | Concrete functor and monad transformers | `algebraic-graphs` |

