module Utils
where

infixr 1 ?
(?) :: Bool -> a -> a -> a
(?) a  b c = if a then b else c

