{-# LANGUAGE Safe #-}

module Data.Duoid.Laws
  ( Law (..),
    checkLaw,
    interchange,
    joinUnit,
    splitUnit,
    swapUnit,
    unswapUnit,
  )
where

import "base" Data.Bool (Bool)
import "base" Data.Kind (Type)
import "this" Data.Duoid (Duoid, pempty, sempty, (<->), (<|>))
import "this" Data.Duoid qualified as Duoid (Normal)

-- | There is a fourth law, but due to the way `Applicative`
type Law :: Type -> Type -> Type
data Law a b = Law {cmp :: b -> b -> Bool, x :: a -> b, y :: a -> b}

checkLaw :: Law a b -> a -> Bool
checkLaw law a = cmp law (x law a) (y law a)

interchange :: (Duoid a) => (a -> a -> Bool) -> Law (a, a, a, a) a
interchange fn =
  Law
    fn
    (\(a, b, c, d) -> (a <-> b) <|> (c <-> d))
    (\(a, b, c, d) -> (a <|> c) <-> (b <|> d))

splitUnit :: (Duoid a) => (a -> a -> Bool) -> Law () a
splitUnit fn = Law fn (\() -> pempty) (\() -> pempty <-> pempty)

joinUnit :: (Duoid a) => (a -> a -> Bool) -> Law () a
joinUnit fn = Law fn (\() -> sempty <|> sempty) (\() -> sempty)

swapUnit :: (Duoid a) => (a -> a -> Bool) -> Law () a
swapUnit fn = Law fn (\() -> pempty) (\() -> sempty)

-- | Additional law for /normal/ duoids.
unswapUnit :: (Duoid.Normal a) => (a -> a -> Bool) -> Law () a
unswapUnit fn = Law fn (\() -> sempty) (\() -> pempty)
