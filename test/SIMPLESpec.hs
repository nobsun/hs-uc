module SIMPLESpec where

import Language.SIMPLE.AbstractSyntax
import Test.Hspec

spec :: Spec
spec = do 
  { describe "isAtom" $ do
    { it "isAtom (Number undefined)" 
        $ isAtom (Number undefined) `shouldBe` True
    }
  }

