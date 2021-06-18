module ParserSpec where

import Control.Monad (liftM2, replicateM)
import Data.Either (fromRight, isLeft)
import qualified Data.List as List
import qualified Parser
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

newtype SingleChar = SingleChar {getSingleChar :: Char}

newtype AnyNonParenthesis = AnyNonParenthesis {getAnyNonParenthesis :: String}
  deriving (Eq, Show)

newtype ASTString = ASTString {getASTString :: String} deriving (Eq, Show)

instance Arbitrary SingleChar where
  arbitrary =
    oneof $
      map
        (pure . SingleChar)
        ( ['A' .. 'J']
            ++ ['a' .. 'j']
            ++ ['0' .. '9']
            ++ ".,:;'/<>?~!@#$%^&*-_+|\\"
        )

instance Arbitrary AnyNonParenthesis where
  arbitrary = AnyNonParenthesis <$> sized name'
    where
      name' n
        | n > 5 = name' 5
        | n == 0 = (: []) . getSingleChar <$> arbitrary
        | n > 0 = liftM2 (:) (getSingleChar <$> arbitrary) (name' (n - 1))
        | otherwise = error "negative size"

instance Arbitrary ASTString where
  arbitrary = ASTString <$> sized formula'
    where
      formula' 0 = leaf'
      formula' n
        | n > 0 = oneof [leaf', node' n]
        | otherwise = error "negative size"
      leaf' = getAnyNonParenthesis <$> arbitrary
      node' n =
        (\x -> "(" ++ x ++ ")")
          <$> liftM2
            (++)
            ((++ " ") . getAnyNonParenthesis <$> arbitrary)
            (List.unwords <$> replicateM (min n 3) (formula' (n `div` 2)))

instance Arbitrary Parser.ParseTree where
  arbitrary = sized tree'
    where
      tree' 0 = leaf'
      tree' n
        | n > 0 = oneof [leaf', node' n]
        | otherwise = error "negative size"
      leaf' = (`Parser.Node` []) . getAnyNonParenthesis <$> arbitrary
      node' n =
        liftM2
          Parser.Node
          (getAnyNonParenthesis <$> arbitrary)
          (replicateM (min n 3) (tree' (n `div` 2)))

spec :: Spec
spec = do
  describe "Parser.parseTree" $ do
    it "handles unwrapped leaf" $
      do
        show (fromRight undefined . Parser.parseTree $ "x" :: Parser.ParseTree)
        `shouldBe` "x"
    it "handles wrapped leaf" $
      do
        show
          ( fromRight undefined . Parser.parseTree $
              " ((( ( x ))  ) ) " ::
              Parser.ParseTree
          )
        `shouldBe` "x"
    it "handles unwrapped node" $
      do
        show
          ( fromRight undefined . Parser.parseTree $ "+ X Y" :: Parser.ParseTree
          )
        `shouldBe` "(+ X Y)"
    it "handles wrapped leaf" $
      do
        show
          ( fromRight undefined . Parser.parseTree $
              " (( ( ( ( ( + X Y )) ))) )  " ::
              Parser.ParseTree
          )
        `shouldBe` "(+ X Y)"
    it "handles named operators" $
      do
        show
          ( fromRight undefined . Parser.parseTree $ "O X Y" :: Parser.ParseTree
          )
        `shouldBe` "(O X Y)"
    it "handles wrapped operator identifiers" $
      do
        show
          ( fromRight undefined
              . Parser.parseTree
              $ "(+) X Y" ::
              Parser.ParseTree
          )
        `shouldBe` "(+ X Y)"
    it "handles wrapped named operators" $
      do
        show
          ( fromRight undefined
              . Parser.parseTree
              $ "(O) X Y" ::
              Parser.ParseTree
          )
        `shouldBe` "(O X Y)"
    it "handles symbol operator with empty parameters" $
      do
        show
          (fromRight undefined . Parser.parseTree $ "+" :: Parser.ParseTree)
        `shouldBe` "+"
    it "handles leaves named by numbers" $
      do
        show
          (fromRight undefined . Parser.parseTree $ "+ X 1" :: Parser.ParseTree)
        `shouldBe` "(+ X 1)"
    it "handles nodes named by numbers" $
      do
        show
          (fromRight undefined . Parser.parseTree $ "1 X Y" :: Parser.ParseTree)
        `shouldBe` "(1 X Y)"

    prop "show . parseTree == id (for our inputs)" $
      \x ->
        show
          ( fromRight undefined . Parser.parseTree $
              getASTString x ::
              Parser.ParseTree
          )
          == getASTString x
    prop "parseTree . show == id" $
      \x ->
        fromRight undefined (Parser.parseTree (show x))
          == (x :: Parser.ParseTree)
