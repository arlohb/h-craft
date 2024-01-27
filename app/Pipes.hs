module Pipes where

(|>) :: a -> (a -> b) -> b
infixl 9 |>
(|>) x f = f x

(||>) :: Functor f => f a -> (a -> b) -> f b
infixl 9 ||>
(||>) xs f = fmap f xs

