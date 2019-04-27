module Test.Spec.UtilsSpec where

import Prelude
import Main.Utils (pow2)
import Test.Spec (describe, it, Spec)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = 
	describe "Utils spec" do 

		describe "general functions" do 
			it "pow2 2 equals 2" do
				let res = pow2 2
				res `shouldEqual` 4

			it "negate 1 equals -1" do
				let res = negate 1
				res `shouldEqual` -1