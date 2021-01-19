{-# LANGUAGE TypeOperators #-}
--From https://riptutorial.com/haskell/example/25923/polynomial-functors

module FunctorKit where

newtype I a = I a
instance Functor I where
    fmap f (I x) = I (f x)

newtype K c a = K c
instance Functor (K c) where
    fmap _ (K c) = K c

infixl 7 :×:
data (f :×: g) a = f a :×: g a
instance (Functor f, Functor g) => Functor (f :×: g) where
    fmap f (fx :×: gy) = fmap f fx :×: fmap f gy

infixl 6 :+:
data (f :+: g) a = InL (f a) | InR (g a)
instance (Functor f, Functor g) => Functor (f :+: g) where
    fmap f (InL fx) = InL (fmap f fx)
    fmap f (InR gy) = InR (fmap f gy)

