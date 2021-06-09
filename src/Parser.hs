{-# LANGUAGE FlexibleContexts #-}

module Parser where

import Control.Applicative (liftA2)
import Data.Sequence as Seq
import Text.Parsec

data AST a
  = Variable
      { variableName :: String
      }
  | Operator
      { operatorName :: String,
        args :: Seq.Seq (AST a)
      }
  deriving (Eq, Show)

ast :: Stream s m Char => ParsecT s u m (AST a)
ast = variable <|> operator

variable :: Stream s m Char => ParsecT s u m (AST a)
variable = Variable <$> identifier <?> "single variable"

operator :: Stream s m Char => ParsecT s u m (AST a)
operator =
  liftA2 Operator operatorString arguments

arguments :: Stream s m Char => ParsecT s u m (Seq.Seq (AST a))
arguments =
  Seq.fromList
    <$> parenthesized (spaced (ast `sepEndBy` many1 space))
    <?> "args list in parentheses"

operatorString :: Stream s m Char => ParsecT s u m String
operatorString =
  spaced
    ( symbols
        <|> parenthesized (spaced identifier <?> "named operator")
        <?> "operator"
    )

identifier :: Stream s m Char => ParsecT s u m String
identifier = many1 letter <> many alphaNum <?> "identifier"

symbols :: Stream s m Char => ParsecT s u m String
symbols = many1 symbol

symbol :: Stream s m Char => ParsecT s u m Char
symbol = oneOf ".,:;'/<>?~!@#$%^&*-+=|\\"

parenthesized :: Stream s m Char => ParsecT s u m t -> ParsecT s u m t
parenthesized = between (char '(') (char ')')

spaced :: Stream s m Char => ParsecT s u m t -> ParsecT s u m t
spaced = between spaces spaces