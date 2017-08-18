module ToBitCodeSpec where

import Test.Tasty.Hspec
import Test.Tasty.QuickCheck as QC

import qualified Data.BitCode.LLVM.Type as T
import qualified Data.BitCode.LLVM.Value as V
import qualified Data.Map.Strict as Map
import Data.BitCode.LLVM.ToBitCode (lookupSymbolIndex)

import Data.List (sortBy)

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

-- prop_typeOrdEq :: T.Ty -> Bool
-- prop_typeOrdEq x = x `compare` x == EQ

spec_bitcode :: Spec
spec_bitcode = do
  describe "types" $ do
    context "typeCompared" $ do
      let i32 = T.Int 32
          i8  = T.Int 8
          i8ptr = T.Ptr 0 i8
          i8ptrptr = T.Ptr 0 i8ptr
          i32ptr = T.Ptr 0 i32
          f = T.Function False i32 [i32, i8ptrptr]
          a = T.Array 13 i8
          aptr = T.Ptr 0 a
          f2 = T.Function True i32 [i8ptr]
          f3 = T.Function False i32 [i8ptr]
          f2ptr = T.Ptr 0 f2
          fptr = T.Ptr 0 f
          
      it "should order function types prior to the function" $ do
        f `T.typeCompare` i32 `shouldBe` GT
        i32 `T.typeCompare` f `shouldBe` LT
        f `T.typeCompare` i8ptr `shouldBe` GT
        i8ptr `T.typeCompare` f `shouldBe` LT
        f `T.typeCompare` i8 `shouldBe` GT
        i8 `T.typeCompare` f `shouldBe` LT
        f `T.typeCompare` i8ptrptr `shouldBe` GT
        i8ptrptr `T.typeCompare` f `shouldBe` LT

      it "should sort correctly" $ do
        sortBy T.typeCompare [i32ptr,f,i8,i8ptr,i8ptrptr, i32]
          `shouldBe` [i8,i32,i8ptr,i32ptr,i8ptrptr,f]
        sortBy T.typeCompare [ i8
                             , i32
                             , i8ptr
                             , a
                             , f2
                             , f3
                             , f
                             , f2ptr
                             , i8ptrptr
                             , fptr
                             , aptr
                             ]
          `shouldBe` [ i8
                     , i32
                     , i8ptr
                     , a
                     , aptr
                     , i8ptrptr
                     , f3
                     , f2
                     , f2ptr
                     , f
                     , fptr ]
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
