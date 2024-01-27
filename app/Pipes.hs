module Pipes where

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

(||>) :: Functor f => f a -> (a -> b) -> f b
(||>) xs f = fmap f xs

