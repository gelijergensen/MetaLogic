module ErrorHandling (Error, parseError, interpretError) where

import qualified Text.Parsec as Parsec

data Error
  = ParseError Parsec.ParseError
  | InterpretError String
  deriving (Show)

parseError :: Parsec.ParseError -> Error
parseError = ParseError

interpretError :: String -> Error
interpretError = InterpretError
