module ToBitCodeSpec where

import Test.Tasty.Hspec
import Test.Tasty.QuickCheck as QC

import qualified Data.BitCode.LLVM.Type as T
import qualified Data.BitCode.LLVM.Value as V
import qualified Data.Map.Strict as Map
import Data.BitCode.LLVM.ToBitCode (lookupSymbolIndex)

import GHC.Stack (HasCallStack)


instance Arbitrary T.Ty where
  arbitrary = oneof [ T.NumEntry <$> arbitrary
                    , pure T.Void
                    , pure T.Float
                    , pure T.Double
                    , pure T.Label
                    , T.Opaque <$> arbitrary
                    , T.Int <$> arbitrary
                    , T.Ptr <$> arbitrary <*> arbitrary
                    , pure T.Half
                    , T.Array <$> arbitrary <*> arbitrary
                    , T.Vector <$> arbitrary <*> arbitrary
                    , pure T.X86Fp80
                    , pure T.Fp128
                    , pure T.Metadata
                    , pure T.X86Mmx
                    , T.StructAnon <$> arbitrary <*> arbitrary
                    , T.StructNamed <$> arbitrary <*> arbitrary <*> arbitrary
                    , T.Function <$> arbitrary <*> arbitrary <*> arbitrary
                    , pure T.Token
                    ]

prop_typeOrdEq :: T.Ty -> Bool
prop_typeOrdEq x = x `compare` x == EQ

spec_bitcode :: Spec
spec_bitcode = do
--  describe "types" $ do
--    when "`compare`ed" $ do
--      it "return EQ" $ property $
--        \x -> x `compare` x == EQ
        -- (T.NumEntry 1 `compare` T.NumEntry 1) `shouldBe` EQ
        -- (T.Void       `compare` T.Void)       `shouldBe` EQ
        -- (T.Float      `compare` T.Float)      `shouldBe` EQ
        -- (T.Double     `compare` T.Double)     `shouldBe` EQ
        -- (T.Label      `compare` T.Label)      `shouldBe` EQ
        -- (T.Opaque "x" `compare` T.Opaque "x") `shouldBe` EQ
        -- (T.Int 1      `compare` T.Int 1)      `shouldBe` EQ
        -- (T.Ptr 1 T.Void `compare` T.Ptr 1 T.Void) `shouldBe` EQ
        -- (T.Ptr 1 (T.Int 1) `compare` (T.Ptr 1 (T.Int 1))) `shouldBe` EQ
        -- (T.Half       `compare` T.Half)       `shouldBe` EQ
        -- (T.Array 0 T.Void) `compare` (T.Array 0 T.Void) `shouldBe` EQ
        -- (T.Vector 0 T.Void) `compare` (T.Vector 0 T.Void) `shouldBe` EQ
        -- (T.X86Fp80    `compare` T.X86Fp80)    `shouldBe` EQ
        -- (T.Fp128      `compare` T.Fp128)      `shouldBe` EQ
        -- (T.Metadata   `compare` T.Metadata)   `shouldBe` EQ
        -- (T.X86Mmx     `compare` T.X86Mmx)     `shouldBe` EQ
        -- (T.StructAnon False [] `compare` T.StructAnon False []) `shouldBe` EQ
        -- (T.StructNamed "x" False [] `compare` T.StructNamed "x" False []) `shouldBe` EQ
        -- (T.Function False T.Void [] `compare` T.Function False T.Void []) `shouldBe` EQ
        -- (T.Token      `compare` T.Token) `shouldBe` EQ
      
  describe "values" $ do
    return ()
  describe "lookupSymbolIndex" $ do
    it "should find the right index for unnamed constants" $ do
      let vals = Map.fromList [(V.Unnamed (V.Constant (T.Int 64) V.Undef), 1)
                              ,(V.Unnamed (V.Constant (T.Int 32) V.Undef), 2)
                              ,(V.Unnamed (V.Constant (T.Int 64) (V.Int 1)), 3)]
      let c1 = V.Unnamed (V.Constant (T.Int 64) V.Undef)
          c2 = V.Unnamed (V.Constant (T.Int 32) V.Undef)
          c3 = V.Unnamed (V.Constant (T.Int 64) (V.Int 1))

      flip Map.lookup vals c1 `shouldBe` Just 1
      lookupSymbolIndex vals c2 `shouldBe` 2
      lookupSymbolIndex vals c3 `shouldBe` 3
