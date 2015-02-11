{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : Data_Types

Menthors    : prof. dr. Andrej Bauer and asist. dr. Matija Pretnar

Authors     : Petra Poklukar, Jure Kukovec

Definitons of fundamental datatypes used in "SearchMonad", "Find_X" and "Functions".
-}

module Data_Types where


data Bit = Zero | One deriving (Eq,Show)

type Natural = Integer
type Cantor = Natural -> Bit

-- | Returns the representation of the 'Bit' sequence obtained by prepending x to the 'Bit' sequence represented by a.
(#) :: Bit -> Cantor -> Cantor
x # a = \i -> if i == 0 then x else a(i-1)

-- | Leafless binary tree data type. Each tree has a node and two children, who are again trees.
data T x = B x (T x) (T x)

-- | Sequence-to-tree isomorphism.
code :: (Natural -> x) -> T x
code f = B (f 0) (code(\n -> f(2*n+1))) (code(\n -> f(2*n+2)))

-- |  Inverse of 'code'.
decode :: T x -> (Natural -> x)
decode (B x l r) n |  n == 0    = x
                   |  odd n     = decode l ((n-1) `div` 2)
                   |  otherwise = decode r ((n-2) `div` 2)

-- | The identity composition of 'decode' and 'code'.
id' :: (Natural -> x) -> (Natural -> x)
id' = decode.code
