module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)
import Data.Int (pow)

--general functions
pow2 :: Int -> Int
pow2 n = pow n 2

xPlusY :: Int -> Int
xPlusY x =  (20 * x) + (10 * x)

--Test 1
test1a :: Int -> Int
test1a v = 16 * (pow2 v) - (24 * v) + 9

test1b :: Int -> Int
test1b v = ((4 * v) - 3) * ((4 * v) - 3)

--Test 2
test2a :: Int -> Int
test2a a = 50 * (pow2 a) - 2

test2b :: Int -> Int
test2b a = 2 * (25 * (pow2 a) - 1)

test2c :: Int -> Int
test2c a = 2 * ((5 * a - 1) * (5 * a + 1))

--Test 3
test3a :: Int -> Int
test3a n = 4 * (pow2 n) + (32 * n) + 64

test3b :: Int -> Int
test3b n = 4 * ((pow2 n) + (8 * n) + 16)

test3c :: Int -> Int
test3c n = ((n + 4) * (n + 4)) * 4

--Test 4
test4a :: Int -> Int
test4a r = 9 * (pow2 r) - 16

test4b :: Int -> Int
test4b r = (3 * r - 4) * (3 * r + 4)

--Test 5
test5a :: Int -> Int
test5a x = 4 * (pow2 x) - 36

test5b :: Int -> Int
test5b x = (2 * x - 6) * (2 * x + 6)

--Test 6
test6 :: Int -> Int
test6 n = -6 * (pow2 n) + (25 * n) - 24

test6a :: Int -> Int
test6a n = -1 * ((pow2 n) - 25 + 144)

--Test 7
test7a :: Int -> Int
test7a b = 60 * (pow2 b) - (6 * b) - 54

test7b :: Int -> Int
test7b b = 6 * (10 * (pow2 b) - (1 * b) - 9)

test7c :: Int -> Int
test7c b = 6 * ((10 * b + 9) * (b - 1))

--Test 8
test8a :: Int -> Int
test8a k = 9 * (pow2 k) + (43 * k) - 10

test8b :: Int -> Int
test8b k = (pow2 k) + (43 * k) - 90

test8c :: Int -> Int
test8c k = (k + 5) * (9 * k - 2)

--Test 9
test9a :: Int -> Int
test9a a = -45 * (pow2 a) - (190 * a) + 175

test9b :: Int -> Int
test9b a = -5 * (9 * (pow2 a) + (38 * a) -35)

test9c :: Int -> Int
test9c a = -5 * ((a + 5) * (9 * a - 7))

--Test 10
test10a :: Int -> Int
test10a n = 24 * (pow2 n) + (150 * n) + 36

test10b :: Int -> Int
test10b n = 6 * (4 * (pow2 n) + (25 * n) + 6)

test10c :: Int -> Int
test10c n = 6 * (n + 6) * (4 * n + 1)

main :: Effect Unit
main = run [consoleReporter] do
  describe "Algebra1" do 

    describe "general functions" do 
      it "pow2 2 equals 2" do
        let res = pow2 2
        res `shouldEqual` 4

      it "negate 1 equals -1" do
        let res = negate 1
        res `shouldEqual` -1

    describe "general functions part 2" do
      it "xPlusY 5 of 20n + 10n equals 150" do
        let res = xPlusY 5
        res `shouldEqual` 150

    describe "test1: 16v^2 - 24v + 9" do
      it "test1a 2 of 16v^2 - 24v + 9 equals 25" do
        let res = test1a 2
        res `shouldEqual` 25
      
      it "test1b 2 of (4v - 3)^2 equals 25" do
        let res = test1b 2
        res `shouldEqual` 25

    describe "test2: 50a^2 - 2" do
      it "test2a 2 of 50a^2 - 2 equals 198" do
        let res = test2a 2
        res `shouldEqual` 198
      
      it "test2b 2 of 2(25a^2 - 1) equal 198" do
        let res = test2b 2
        res `shouldEqual` 198
      it "test2c 2 of 2(5a - 1)(5a + 1) equal 198" do
        let res = test2c 2
        res `shouldEqual` 198

    describe "test3: 4n^2 + 32n + 64" do
      it "test3a 2 of 4n^2 + 32n + 64 equals 144" do
        let res = test3a 2
        res `shouldEqual` 144
      
      it "test3b 2 of 4 * (n^2 + 8n + 16) equals 144" do
        let res = test3b 2
        res `shouldEqual` 144

      it "test3c 2 of 4*(n + 4)^2 equals 144" do
        let res = test3c 2
        res `shouldEqual` 144

    describe "test4: 9r^2 - 16" do
      it "test4a 2 of 9r^2 - 16 equal 20" do
        let res = test4a 2
        res `shouldEqual` 20
      
      it "test4b 2 of (3r - 4) (3r + 4) equal 20" do
        let res = test4b 2
        res `shouldEqual` 20

    describe "test5: 4x^2 - 36" do
      it "test5a 2 of 4x^2 - 36 equal -20" do
        let res = test5a 2
        res `shouldEqual` -20

      it "test5b 2 of (2x - 6)(2x + 6) equal -20" do
        let res = test5b 2
        res `shouldEqual` -20

    describe "test6: -6n^2+25n-24" do  
      it "-6n^2+25n-24 equals -20" do
        let actual = test6 4
        let expected = -20
        actual `shouldEqual` expected

      it "-((pow2 n) - 25 + 144) equals -20" do
        let res = test6 4
        res `shouldEqual` -20

    describe "test7: 60b^2 - 6b - 54" do
      it "test7a 2 of 60b^2 - 6b - 54 equal 174" do
        let res = test7a 2
        res `shouldEqual` 174

      it "test7b 2 of 6(10b^2 - 1b - 9) equal 174" do
        let res = test7b 2
        res `shouldEqual` 174
      
      it "test7c 2 of 6(10b + 9)(b - 1) equal 174" do
        let res = test7c 2
        res `shouldEqual` 174

    describe "test8: 9k^2 + 43k - 10" do
      it "test8a 2 of 9k^2 + 43k - 10 equal 112" do
        let res = test8a 2
        res `shouldEqual` 112

      it "test8b 2 of k^2 + 43k - 90 equal 0" do
        let res = test8b 2
        res `shouldEqual` 0
      
      it "test8c 2 of (k + 5)(k - 2) equal 112" do
        let res = test8c 2
        res `shouldEqual` 112
    
    describe "test9: -45a^2 - 190a + 175" do
      it "test9a 2 of -45a^2 - 190a +175 equal -385" do
        let res = test9a 2
        res `shouldEqual` -385
      
      it "test9b 2 of -5(9a^2 +38a - 35) equal -385" do
        let res = test9b 2
        res `shouldEqual` -385

      it "test9c 2 of -5(a - 5)(45a - 7) equal -385" do
        let res = test9c 2
        res `shouldEqual` -385

    describe "test10: 24n^2 + 150n + 36" do
      it "test10a 2 of 24n^2 + 150n +36 equal 432" do
        let res = test10a 2
        res `shouldEqual` 432

      it "test10b 2 of 6(4n^2 +25n + 6 equal 432" do
        let res = test10b 2
        res `shouldEqual` 432

      it "test10c 2 of 6(n + 6)(4n + 1) equal 432" do
        let res = test10c 2
        res `shouldEqual` 432