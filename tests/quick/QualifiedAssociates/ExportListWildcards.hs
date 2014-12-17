module ExportListWildcards (Foo(..), Bar(..), N(..)) where

data Foo = Foo1 | Foo2 Int | Foo3 { c :: Bool }

class Bar a where x :: a -> Foo

newtype N = N { unN :: Foo }

a = a

data Baz = Baz
