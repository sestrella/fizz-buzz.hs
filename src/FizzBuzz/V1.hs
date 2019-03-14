{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}

module FizzBuzz.V1 where

import Data.Type.Bool
import Data.Type.Equality
import GHC.TypeNats

data FizzBuzz
  = Fizz
  | Buzz
  | FizzBuzz
  | Number Nat

type ToFizzBuzz a = If (DivisibleBy5 a && DivisibleBy3 a) 'FizzBuzz (ToFizz a)

type ToFizz a = If (DivisibleBy3 a) Fizz (ToBuzz a)

type ToBuzz a = If (DivisibleBy5 a) Buzz (Number a)

type DivisibleBy3 a = DivisibleBy 3 a

type DivisibleBy5 a = DivisibleBy 5 a

type DivisibleBy a b = Mod b a == 0

testFizz :: ToFizzBuzz 3 :~: Fizz
testFizz = Refl

testBuzz :: ToFizzBuzz 5 :~: Buzz
testBuzz = Refl

testFizzBuzz :: ToFizzBuzz 15 :~: 'FizzBuzz
testFizzBuzz = Refl

testNumber :: ToFizzBuzz 1 :~: Number 1
testNumber = Refl
