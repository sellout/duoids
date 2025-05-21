{-# LANGUAGE Safe #-}

-- | Support for various combinations of @do@-notation for `Duoid` instances.
--
--   The ideal use case for this module is in combination with @ApplicativeDo@
--   and @QualifiedDo@. In that case, you can get duoidal semantics out of @do@
--   notatation on a case-by-case basis.
--
--   If you don't have at least GHC 9.0, then you /could/ use @RebindableSyntax@
--   instead of @QualifiedDo@. However, that changes the semantics and
--   availability for every use of the exported operators in the entire module.
--
--   Additionally, if you /don't/ enable @ApplicativeDo@, then you will simply
--   have monadic semantics instead of duoidal semantics. However, this can
--   still be nice for avoiding the need for using the `Sequential` @newtype@
--   required by unqualified @do@ notation. Since @ApplicativeDo@ is a bit
--   buggy, this may be good enough for some use cases.
--
--  ## Example
--
--   When the independent steps succeed, we just return the dependent result,
--   like a `Monad`.
--
-- >>> :{
-- Duoidal.do
--   x <- Right "x"
--   y <- Right "y"
--   Left ["Couldn't merge records " <> x <> " and " <> y]
-- :}
-- Left ["Couldn't merge records x and y"]
--
--   But when the independent steps fail, we collect all the independent
--   failures, like `Validation`.
--
-- >>> :{
-- Duoidal.do
--   x <- Left ["Couldn't find record x"]
--   y <- Left ["Couldn't find record y"]
--   Left ["Couldn't merge records " <> x <> " and " <> y]
-- :}
-- Left ["Couldn't find record x","Couldn't find record y"]
--
--  __NB__: `fail` isn’t part of the `Duoidal` interface, so this just
--          re-exports the one from `MonadFail`.
module Control.Duoidal.Do
  ( fail,
    fmap,
    join,
    pure,
    return,
    (>>),
    (>>=),
    (<*>),
  )
where

import "base" Control.Monad.Fail (fail)
import "base" Data.Functor (fmap)
import "this" Control.Duoidal (Duoidal, joinD, pureD, (<*\>), (>>\=), (>\>))

-- $setup
-- >>> :seti -XApplicativeDo
-- >>> :seti -XQualifiedDo
-- >>> import "base" Data.Either (Either (Left, Right))
-- >>> import "base" Data.Semigroup ((<>))
-- >>> import qualified "duoids" Control.Duoidal.Do as Duoidal

(>>) :: (Duoidal f) => f a -> f b -> f b
(>>) = (>\>)

(>>=) :: (Duoidal f) => f a -> (a -> f b) -> f b
(>>=) = (>>\=)

(<*>) :: (Duoidal f) => f (a -> b) -> f a -> f b
(<*>) = (<*\>)

join :: (Duoidal f) => f (f a) -> f a
join = joinD

pure :: (Duoidal f) => a -> f a
pure = pureD

return :: (Duoidal f) => a -> f a
return = pureD

-- -- | A "sequence" in category theory is any morphism whose domain is a subobject
-- --   of the natural numbers object. If not for the subobject requirement, we
-- --   might define something like
-- --
-- -- > type Sequence c a = NNO c `c` a
-- --
-- --   However, we need to support different domains for diffeent data structures
-- --   that represent sequences.
-- class Sequence (cat :: ok -> ok -> Type) (s :: ok -> Type) where
--   -- | Should represent a subset of `NNO`.
--   type Domain cat s :: ok
--   index :: forall (a :: ok). s ok -> Domain cat s `cat` a

-- instance Sequence (->) (Vec n) where
--   type Domain (->) (Vec n) = Fin n
--   index = (!)

-- -- | This one is partial.
-- instance Sequence (->) [] where
--   type Domain (->) [] = Natural
--   index xs = (xs !!) . fromIntegral

-- newtype NTList f = forall a. NTList {unNTList :: [f a]}

-- -- | This one is partial.
-- instance Sequence (NaturalTransformation (->)) NTList where
--   type Domain (->) NTList = Const Natural
--   index xs = NT $ (xs !!) . fromIntegral . getConst
