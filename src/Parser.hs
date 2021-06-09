{-# LANGUAGE FlexibleContexts #-}

module Parser where

import Control.Applicative (liftA2)
import Text.Parsec

data AST a
  = Leaf
      { leafName :: String
      }
  | Node
      { name :: String,
        args :: [AST a]
      }
  deriving (Eq, Show)

ast :: Stream s m Char => ParsecT s u m (AST a)
ast = maybeWrapped (leaf <|> node)

leafOrNode :: Stream s m Char => ParsecT s u m (AST a)
leafOrNode = maybeSpaced leaf <|> wrapped (maybeWrapped node)

leaf :: Stream s m Char => ParsecT s u m (AST a)
leaf = Leaf <$> identifier

node :: Stream s m Char => ParsecT s u m (AST a)
node = liftA2 f nodeName arguments
  where
    f n [] = Leaf n
    f n xs = Node n xs

nodeName :: Stream s m Char => ParsecT s u m String
nodeName = maybeWrapped (operator <|> identifier)

identifier :: Stream s m Char => ParsecT s u m String
identifier = many1 letter <> many alphaNum <?> "identifier"

operator :: Stream s m Char => ParsecT s u m String
operator = many1 symbol <?> "operator"

symbol :: Stream s m Char => ParsecT s u m Char
symbol = oneOf ".,:;'/<>?~!@#$%^&*-+=|\\"

arguments :: Stream s m Char => ParsecT s u m [AST a]
arguments = many leafOrNode

-- For flexibility with spaces and nested parentheses
maybeWrapped :: Stream s m Char => ParsecT s u m t -> ParsecT s u m t
maybeWrapped p = try (maybeSpaced p) <|> wrapped (maybeWrapped p)

wrapped :: Stream s m Char => ParsecT s u m t -> ParsecT s u m t
wrapped = maybeSpaced . parenthesized . maybeSpaced

parenthesized :: Stream s m Char => ParsecT s u m t -> ParsecT s u m t
parenthesized = between (char '(') (char ')')

maybeSpaced :: Stream s m Char => ParsecT s u m t -> ParsecT s u m t
maybeSpaced = between spaces spaces
