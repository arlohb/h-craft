module Pipes where

(|>) :: a -> (a -> b) -> b
infixl 9 |>
x |> f = f x

(||>) :: Functor f => f a -> (a -> b) -> f b
infixl 9 ||>
xs ||> f = fmap f xs

(!?) :: [a] -> Int -> Maybe a
infixl 9 !?
xs !? n
    | n < 0     = Nothing
    | otherwise =
        foldr (\x r k -> case k of
            0 -> Just x
            _ -> r (k - 1))
            (const Nothing) xs n


