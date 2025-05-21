-- | Support for various combinations of @do@-notation for `Duoid` instances.
--
--   The ideal use case for this module is in combination with @ApplicativeDo@ and @QualifiedDo@. In
--   that case, you can get duoidal semantics out of @do@ notatation on a case-by-case basis.
--
--   If you don't have at least GHC 9.0, then you /could/ use @RebindableSyntax@ instead of
--   @QualifiedDo@. However, that changes the semantics and availability for every use of the
--   exported operators in the entire module.
--
--   Additionally, if you /don't/ enable @ApplicativeDo@, then you will simply have monadic
--   semantics instead of duoidal semantics. However, this can still be nice for avoiding the need
--   for using the `Sequential` @newtype@ required by unqualified @do@ notation. Since
--   @ApplicativeDo@ is a bit buggy, this may be good enough for some use cases.
--
-- > {-# language ApplicativeDo, QualifiedDo #-}
-- > import qualified Duoid.Duoidal.Do as Duoidal
-- > foo :: Semigroup a => Either a b
-- > foo = Duoidal.do
-- >   ...
--
--  __NB__: This doesn't provide any definition for @fail@, as it's not part of the duoidal
--          interface.
module Control.Duoidal.Do
  ( join,
    (>>=),
    (<*>),
    (<$>),
  )
where

import Control.Duoidal (joinD, (<*\>), (>>\=))
import Prelude ((<$>))

(>>) = (>\>)

(>>=) = (>>\=)

(<*>) = (<*\>)

join = joinD

-- | A "sequence" in category theory is any morphism whose domain is a subobject of the natural numbers object. If not for the subobject requirement, we might define something like
--
-- > type Sequence c a = NNO c `c` a
--
--   However, we need to support different domains for diffeent data structures that represent sequences.
class Sequence (cat :: ok -> ok -> Type) (s :: ok -> Type) where
  -- | Should represent a subset of `NNO`.
  type Domain cat s :: ok

  index :: forall (a :: ok). s ok -> Domain cat s `cat` a

instance Sequence (->) (Vec n) where
  type Domain (->) (Vec n) = Fin n
  index = (!)

-- | This one is partial.
instance Sequence (->) [] where
  type Domain (->) [] = Natural
  index xs = (xs !!) . fromIntegral

newtype NTList f = forall a. NTList {unNTList :: [f a]}

-- | This one is partial.
instance Sequence (NaturalTransformation (->)) NTList where
  type Domain (->) NTList = Const Natural
  index xs = NT $ (xs !!) . fromIntegral . getConst
