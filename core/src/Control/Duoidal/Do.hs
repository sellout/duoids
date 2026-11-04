{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- Re-export all of the operations used by @do@ notation (including with
-- [@ApplicativeDo@](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/applicative_do.html)
-- and
-- [@RecursiveDo@](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/recursive_do.html)).
-- This way you can use
--
-- > :seti -XApplicativeDo
-- > :seti -XQualifiedDo
-- > import "duoids" Control.Duoidal.Do qualified as Duoidal
--
-- and get duoidal semantics with @Duoidal.do@. Alternatively,
--
-- > :seti -XApplicativeDo
-- > :seti -XRebindableSyntax
-- > import "duoids" Control.Duoidal.Do -- intentionally omitted import list
--
-- to get duoidal semantics with /all/ @do@ blocks. I would use this approach
-- myself if
-- [@RebindableSyntax@](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/rebindable_syntax.html)
-- were more fine-grained.
--
-- @since 99999
module Control.Duoidal.Do
  ( fail,
    fmap,
    join,
    mfix,
    return,
    (<*>),
    (>>),
    (>>=),
  )
where

import "base" Data.Functor (fmap)
import "this" Control.Duoidal (join, return, (<*>), (>>), (>>=))
import "this" Control.Duoidal.Fail (fail)
import "this" Control.Duoidal.Fix (mfix)
