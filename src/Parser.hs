{-# LANGUAGE FlexibleContexts #-}

module Parser where

import qualified AbstractSyntaxTree as AST
import Control.Monad (liftM2)
import qualified Data.Char as Char
import qualified Data.List as List
import Text.Parsec
import Text.Parsec.Pos (updatePosString)

data ParseTree
  = Leaf
      { leafName :: String
      }
  | Node
      { name :: String,
        args :: [ParseTree]
      }
  deriving (Eq)

data OperatorName
  = Symbols String
  | Identifier String

instance Show ParseTree where
  show (Leaf x) = x
  show (Node x ys) = "(" ++ x ++ " " ++ List.unwords (map show ys) ++ ")"

parseAST :: String -> Either ParseError (AST.AST String)
parseAST str = toAST <$> parseTree str
  where
    toAST (Leaf n) = AST.ast n []
    toAST (Node n args) = AST.ast n $ map toAST args

parseTree :: String -> Either ParseError ParseTree
parseTree = parse tree "" . tokenize

tree :: Stream s m String => ParsecT s u m ParseTree
tree = maybeWrapped (leaf <|> node)

leafOrNode :: Stream s m String => ParsecT s u m ParseTree
leafOrNode =
  leaf <|> wrapped (maybeWrapped node)
    <?> "variable or subtree in parentheses"

leaf :: Stream s m String => ParsecT s u m ParseTree
leaf = Leaf <$> identifier

node :: Stream s m String => ParsecT s u m ParseTree
node = do
  n <- nodeName
  xs <- arguments
  node' n xs
  where
    node' (Symbols n) [] =
      fail "Likely problem: operators named with symbols need arguments"
    node' (Identifier n) [] = return $ Leaf n
    node' (Symbols n) xs = return $ Node n xs
    node' (Identifier n) xs = return $ Node n xs

nodeName :: Stream s m String => ParsecT s u m OperatorName
nodeName = maybeWrapped (Symbols <$> operator <|> Identifier <$> identifier)

arguments :: Stream s m String => ParsecT s u m [ParseTree]
arguments = many leafOrNode

-- For flexibility with nested parentheses
maybeWrapped :: Stream s m String => ParsecT s u m t -> ParsecT s u m t
maybeWrapped p = try p <|> wrapped (maybeWrapped p)

wrapped :: Stream s m String => ParsecT s u m t -> ParsecT s u m t
wrapped = between (word "(") (word ")")

-- Custom parsers which process individual strings at the char level
identifier :: Stream s m String => ParsecT s u m String
identifier = satisfyWord f
  where
    f x = Char.isLetter (head x) && all Char.isAlphaNum (tail x)

operator :: Stream s m String => ParsecT s u m String
operator = satisfyWord f
  where
    f = all (`elem` ".,:;'/<>?~!@#$%^&*-+=|\\")

word :: Stream s m String => String -> ParsecT s u m String
word w = satisfyWord (== w) <?> w

-- Custom parser for processing tokens of words at the string level
satisfyWord :: Stream s m String => (String -> Bool) -> ParsecT s u m String
satisfyWord f =
  tokenPrim showWord nextPos testWord
  where
    showWord x = "\"" ++ x ++ "\""
    testWord x = if f x then Just x else Nothing
    nextPos pos x xs = updatePosString pos x

-- Break tokens on each whitespace or parenthesis
tokenize :: String -> [String]
tokenize = filter (not . Char.isSpace . head) . List.groupBy f
  where
    f '(' _ = False
    f _ '(' = False
    f ')' _ = False
    f _ ')' = False
    f x y = Char.isSpace x == Char.isSpace y
