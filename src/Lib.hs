{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Data.Type.Bool
import Data.Type.Equality
import GHC.TypeLits

data FizzBuzz
  = Fizz
  | Buzz
  | FizzBuzz
  | Number Nat

type ToFizzBuzz a = If (DivisibleBy a 5 && DivisibleBy a 3) 'FizzBuzz (ToFizz a)

type ToFizz a = If (DivisibleBy a 3) Fizz (ToBuzz a)

type ToBuzz a = If (DivisibleBy a 5) Buzz (Number a)

type DivisibleBy (a :: Nat) (b :: Nat) = Mod a b == 0

divisibleBy3 :: ToFizzBuzz 3 :~: Fizz
divisibleBy3 = Refl

divisibleBy5 :: ToFizzBuzz 5 :~: Buzz
divisibleBy5 = Refl

divisibleBy3And5 :: ToFizzBuzz 15 :~: 'FizzBuzz
divisibleBy3And5 = Refl
