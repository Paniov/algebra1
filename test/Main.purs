module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)
import Data.Int (pow, toNumber, ceil)
import Data.Decimal (fromInt)

pow2 :: Int -> Int
pow2 n = pow n 2

ex1 :: Int -> Int
ex1 n = -6 * (pow2 n) + (25 * n) - 24

ex1a :: Int -> Int
ex1a n = -1 * ((pow2 n) - 25 + 144)

negate :: Int -> Int
negate = (-) 0

-- 20x + 10x
xPlusY :: Int -> Int
xPlusY x =  (20 * x) + (10 * x)

-- 5x - 16y + 30x
gg :: Int -> Int -> Int
gg x y = (5 * x) - (16 * y) + (30 * x)

powX2 :: Int -> Int
powX2 x = 4 * (pow2 x) + (32 * x) + 64

powX2a :: Int -> Int
powX2a x = (pow2 x) + (8 * x) + 16

powX2b :: Int -> Number
powX2b x = 0.5 * (toNumber (pow2 x)) + (toNumber x) + (toNumber 2)

main :: Effect Unit
main = run [consoleReporter] do
  describe "Algebra1" do 
    it "pow2 2 equals 2" do
      let res = pow2 2
      res `shouldEqual` 4

    it "negate 1 equals -1" do
      let res = negate 1
      res `shouldEqual` -1
      
    it "-6n^2+25n-24 equals -20" do
      let emaChampion = ex1 4
      emaChampion `shouldEqual` -20

    it "-((pow2 n) - 25 + 144) equals -20" do
      let res = ex1 4
      res `shouldEqual` -20

    it "xPlusY 5 of 20n + 10n equals 150" do
      let res = xPlusY 5
      res `shouldEqual` 150

    it "gg 6 7 of 5x - 16y + 30y equals 98" do
      let res = gg 6 7
      res `shouldEqual` 98

    it "powX2 2 of 4n^2 + 32n + 64 equals 144" do
      let res = powX2 2
      res `shouldEqual` 144

    it "powX2a 2 of n^2 + 8n + 16 equals 36" do
      let res = powX2a 2
      res `shouldEqual` 36

    it "result of powX2 is less than powX2a in 4 times" do
      let resX = powX2 2
      let resY = powX2a 2
      let res = resX / resY
      res `shouldEqual` 4

    it "powX2b 2 of 0.5 * n^2 + n + 2 equals 6" do
      let res = powX2b 2
      ceil res `shouldEqual` 6