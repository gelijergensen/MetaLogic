{-# LANGUAGE FlexibleContexts #-}

module Parser where

import qualified AbstractSyntaxTree as AST
import Control.Monad (liftM2)
import qualified Data.Char as Char
import qualified Data.List as List
import Text.Parsec
import Text.Parsec.Pos (updatePosString)

data ParseTree = Node
  { name :: String,
    args :: [ParseTree]
  }
  deriving (Eq)

instance Show ParseTree where
  show (Node x []) = x
  show (Node x ys) = "(" ++ x ++ " " ++ List.unwords (map show ys) ++ ")"

parseAST :: String -> Either ParseError (AST.AST String)
parseAST str = toAST <$> parseTree str
  where
    toAST (Node n args) = AST.ast n $ map toAST args

parseTree :: String -> Either ParseError ParseTree
parseTree = parse start "" . tokenize

start :: Stream s m String => ParsecT s u m ParseTree
start = tree <* eof

tree :: Stream s m String => ParsecT s u m ParseTree
tree = maybeWrapped node

leafOrTree :: Stream s m String => ParsecT s u m ParseTree
leafOrTree =
  try leaf <|> wrapped tree <?> "variable or subtree in parentheses"

leaf :: Stream s m String => ParsecT s u m ParseTree
leaf = (`Node` []) <$> anyNonParenthesis

node :: Stream s m String => ParsecT s u m ParseTree
node = liftM2 Node nodeName arguments

nodeName :: Stream s m String => ParsecT s u m String
nodeName = maybeWrapped anyNonParenthesis

arguments :: Stream s m String => ParsecT s u m [ParseTree]
arguments = many leafOrTree

-- For flexibility with nested parentheses
maybeWrapped :: Stream s m String => ParsecT s u m t -> ParsecT s u m t
maybeWrapped p = try p <|> wrapped (maybeWrapped p)

wrapped :: Stream s m String => ParsecT s u m t -> ParsecT s u m t
wrapped = between (word "(") (word ")")

anyNonParenthesis :: Stream s m String => ParsecT s u m String
anyNonParenthesis = satisfyWord (`notElem` ["(", ")"])

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
