{-# LANGUAGE FlexibleContexts #-}

{-|
Modul: Data_Types

Opis:  Definicija Cantorjevega prostora.

Mentorja: asist. dr. Matija Pretnar in prof. dr. Andrej Bauer.

Avtorja: Petra Poklukar, Jure Kukovec

-}

module Data_Types where


data Bit = Zero | One deriving (Eq,Show)

type Natural = Integer
type Cantor = Natural -> Bit

-- | Returns the representation of the Bit sequence obtained by prepending x to the Bit sequence represented by a
(#) :: Bit -> Cantor -> Cantor
x # a = \i -> if i == 0 then x else a(i-1)
