module Monads where

import Data.Monoid 

-- Option/Maybe

data Option a = None | Some a
  deriving (Show, Eq)

instance Functor Option where
  fmap _ None     = None
  fmap f (Some x) = Some $ f x

instance Applicative Option where
  pure = Some
  (Some f) <*> (Some x) = Some (f x)
  _ <*> _               = None

instance Monad Option where
  return = Some
  (Some x) >>= f = f x
  _        >>= _ = None

-- List

data List a = Nil | Cons a (List a)
  deriving (Show, Eq)

foldr' :: (a -> b -> b) -> b -> List a -> b
foldr' _ acc Nil         = acc
foldr' f acc (Cons x xs) = f x (foldr' f acc xs)

concat' :: List (List a) -> List a
concat' = foldr' (<>) Nil

instance Monoid (List a) where
  mempty        = Nil
  mappend xs ys = foldr' Cons ys xs

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  fs <*> xs = concat' $ fmap (`fmap` xs) fs

instance Monad List where
  return x = Cons x Nil
  xs >>= f = concat' (fmap f xs)

