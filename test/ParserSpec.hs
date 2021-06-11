module ParserSpec where

import qualified Assignment
import Control.Monad (liftM2, replicateM)
import Data.Either (fromRight, isLeft)
import qualified Data.List as List
import qualified Parser
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

newtype Alpha = Alpha {getAlpha :: Char}

newtype AlphaNum = AlphaNum {getAlphaNum :: Char}

newtype Identifier = Identifier {getIdentifier :: String}

newtype Symbol = Symbol {getSymbol :: Char}

newtype OperatorName = OperatorName {getOperatorName :: String}

newtype ASTString = ASTString {getASTString :: String} deriving (Eq, Show)

instance Arbitrary Alpha where
  arbitrary = oneof $ map (pure . Alpha) (['A' .. 'Z'] ++ ['a' .. 'z'])

instance Arbitrary AlphaNum where
  arbitrary =
    oneof $ map (pure . AlphaNum) (['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'])

instance Arbitrary Symbol where
  arbitrary = oneof $ map (pure . Symbol) ".,:;'/<>?~!@#$%^&*-+=|\\"

instance Arbitrary Identifier where
  arbitrary =
    Identifier
      <$> liftM2
        (:)
        (getAlpha <$> arbitrary)
        (oneof [pure [], (: []) . getAlphaNum <$> arbitrary])

instance Arbitrary OperatorName where
  arbitrary = OperatorName <$> sized name'
    where
      name' n
        | n > 2 = name' 2
        | n == 0 = (: []) . getSymbol <$> arbitrary
        | n > 0 = liftM2 (:) (getSymbol <$> arbitrary) (name' (n - 1))

instance Arbitrary ASTString where
  arbitrary = ASTString <$> sized formula'
    where
      formula' 0 = leaf'
      formula' n
        | n > 0 = oneof [leaf', node' n]
      leaf' = getIdentifier <$> arbitrary
      node' n =
        (\x -> "(" ++ x ++ ")")
          <$> liftM2
            (++)
            ((++ " ") . getOperatorName <$> arbitrary)
            (List.unwords <$> replicateM (min n 3) (formula' (n `div` 2)))

instance Arbitrary Parser.AST where
  arbitrary = sized tree'
    where
      tree' 0 = leaf'
      tree' n
        | n > 0 = oneof [leaf', node' n]
      leaf' = Parser.Leaf . getIdentifier <$> arbitrary
      node' n =
        liftM2
          Parser.Node
          (getOperatorName <$> arbitrary)
          (replicateM (min n 3) (tree' (n `div` 2)))

spec :: Spec
spec = do
  describe "Parser.parseAST" $ do
    it "handles unwrapped leaf" $
      do
        show (fromRight undefined . Parser.parseAST $ "x" :: Parser.AST)
        `shouldBe` "x"
    it "handles wrapped leaf" $
      do
        show
          ( fromRight undefined . Parser.parseAST $
              " ((( ( x ))  ) ) " ::
              Parser.AST
          )
        `shouldBe` "x"
    it "handles unwrapped node" $
      do
        show (fromRight undefined . Parser.parseAST $ "+ X Y" :: Parser.AST)
        `shouldBe` "(+ X Y)"
    it "handles wrapped leaf" $
      do
        show
          ( fromRight undefined . Parser.parseAST $
              " (( ( ( ( ( + X Y )) ))) )  " ::
              Parser.AST
          )
        `shouldBe` "(+ X Y)"
    it "errors when handling symbol operator with empty parameters" $
      do isLeft $ Parser.parseAST "+"
    it "errors when passed invalid input" $
      do isLeft $ Parser.parseAST "(+ X 1)"
    it "errors when node in subtree is not wrapped in parentheses" $
      do isLeft $ Parser.parseAST "(+ - Y X)"
    prop "show . parseAST == id (for our inputs)" $
      \x ->
        show
          ( fromRight undefined . Parser.parseAST $
              getASTString x ::
              Parser.AST
          )
          == getASTString x
    prop "parseAST . show == id" $
      \x ->
        fromRight undefined (Parser.parseAST (show x)) == (x :: Parser.AST)
