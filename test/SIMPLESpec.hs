module SIMPLESpec where

import Language.SIMPLE.AbstractSyntax
import Test.Hspec

spec :: Spec
spec = do
  { specAbstractSyntax
  }

specAbstractSyntax :: Spec
specAbstractSyntax = do 
  { describe "isAtom" $ do
    { it "isAtom (Number undefined)" 
        $ isAtom (Number undefined) `shouldBe` True
    ; it "isAtom (Variable undefined)"
        $ isAtom (Variable undefined) `shouldBe` True
    ; it "isAtom (Add undefined undefined)"
        $ isAtom (Add undefined undefined) `shouldBe` False
    ; it "isAtom (Multiply undefined undefined)"
        $ isAtom (Multiply undefined undefined) `shouldBe` False
    ; it "isAtom (And undefined undefined)"
        $ isAtom (And undefined undefined) `shouldBe` False
    ; it "isAtom (Or undefined undefined)"
        $ isAtom (Or undefined undefined) `shouldBe` False
    ; it "isAtom (Not undefined)"
        $ isAtom (Not undefined) `shouldBe` False
    ; it "isAtom (LessThan undefined undefined)"
        $ isAtom (LessThan undefined undefined) `shouldBe` False
    }
  ; describe "isCompound" $ do
    { it "isCompound DoNothing"
        $ isCompound DoNothing `shouldBe` False
    ; it "isCompound (Assign undefined undefined)"
        $ isCompound (Assign undefined undefined) `shouldBe` False
    ; it "isCompound (If undefined undefined undefined)"
        $ isCompound (If undefined undefined undefined) `shouldBe` False
    ; it "isCompound (Sequence undefined undefined)"
        $ isCompound (Sequence undefined undefined) `shouldBe` True
    ; it "isCompound (While undefined undefined)"
        $ isCompound (While undefined undefined) `shouldBe` False
    }
  }
