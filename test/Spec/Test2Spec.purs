module Test.Spec.Test2Spec where

import Prelude
import Main.Utils (pow2)
import Test.Spec (pending, describe, it, Spec)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = 
  describe "Algebra1 Test2" do 

    describe "test1: ..." do
      pending "test1 ..."
