{-# LANGUAGE TypeOperators #-}
module TypeOperatorOperator where

data (:><:) a b = (:><:) a b

f :: a :><: b -> b :><: a
f (a :><: b) = b :><: a
