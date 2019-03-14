{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeOperators  #-}

module FizzBuzz.V2 where

import Data.Type.Equality
import GHC.TypeNats

data FizzBuzz
  = Fizz
  | Buzz
  | FizzBuzz
  | Number Nat

type ToFizzBuzz a = MkFizzBuzz a (DivisibleBy3 a) (DivisibleBy5 a)

type family MkFizzBuzz (a :: Nat) (divBy3 :: Bool) (divBy5 :: Bool) :: FizzBuzz where
  MkFizzBuzz _ True True   = 'FizzBuzz
  MkFizzBuzz _ True False  = Fizz
  MkFizzBuzz _ False True  = Buzz
  MkFizzBuzz a False False = Number a

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
