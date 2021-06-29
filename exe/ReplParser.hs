{-# LANGUAGE FlexibleContexts #-}

module ReplParser where

import Data.Functor (($>))
import Text.Parsec

data UserInput
  = Quit
  | Help
  | List
  | DoNothing -- when someone just enters spaces
  | NewSystem String
  | NewFormula String String
  | Step String
  | Rewrite String
  | Undetermined
  deriving (Show)

parseInput :: String -> UserInput
parseInput x = case parse input "repl" x of
  Right res -> res
  Left _ -> Undetermined

input :: Stream s m Char => ParsecT s u m UserInput
input = command <|> doNothing <|> formula

command :: Stream s m Char => ParsecT s u m UserInput
command = char ':' *> tryOneOf [quit, help, list, setSystem, step, rewrite]

tryOneOf :: Stream s m t => [ParsecT s u m a] -> ParsecT s u m a
tryOneOf = choice . map try

quit :: Stream s m Char => ParsecT s u m UserInput
quit =
  ( try
      ( string "quit"
          *> eof
      )
      <|> try
        ( char 'q'
            *> eof
        )
  )
    $> Quit

help :: Stream s m Char => ParsecT s u m UserInput
help =
  ( try
      ( string "help"
          *> eof
      )
      <|> try
        ( char 'h'
            *> eof
        )
  )
    $> Help

list :: Stream s m Char => ParsecT s u m UserInput
list =
  ( try
      ( string "list"
          *> eof
      )
      <|> try
        ( char 'l'
            *> eof
        )
  )
    $> List

setSystem :: Stream s m Char => ParsecT s u m UserInput
setSystem = do
  string "set system"
  spaces
  x <- many (letter <|> space)
  spaces
  eof
  return $ NewSystem x

step :: Stream s m Char => ParsecT s u m UserInput
step = try namedStep <|> unNamedStep

namedStep :: Stream s m Char => ParsecT s u m UserInput
namedStep = do
  string "step"
  x <- between spaces spaces (many1 alphaNumOrUnderscore)
  eof
  return $ Step x

unNamedStep :: Stream s m Char => ParsecT s u m UserInput
unNamedStep = do
  string "step"
  spaces
  eof
  return $ Step "_"

rewrite :: Stream s m Char => ParsecT s u m UserInput
rewrite = try namedRewrite <|> unNamedRewrite

namedRewrite :: Stream s m Char => ParsecT s u m UserInput
namedRewrite = do
  string "rewrite"
  x <- between spaces spaces (many1 alphaNumOrUnderscore)
  eof
  return $ Rewrite x

unNamedRewrite :: Stream s m Char => ParsecT s u m UserInput
unNamedRewrite = do
  string "rewrite"
  spaces
  eof
  return $ Rewrite "_"

doNothing :: Stream s m Char => ParsecT s u m UserInput
doNothing = try (spaces *> eof) $> DoNothing

formula :: Stream s m Char => ParsecT s u m UserInput
formula = try namedFormula <|> unNamedFormula

namedFormula :: Stream s m Char => ParsecT s u m UserInput
namedFormula = do
  name <- between spaces spaces (many1 alphaNumOrUnderscore)
  char '='
  form <- many anyChar
  eof
  return $ NewFormula name form

unNamedFormula :: Stream s m Char => ParsecT s u m UserInput
unNamedFormula = do
  form <- many anyChar
  eof
  return $ NewFormula "_" form

alphaNumOrUnderscore :: Stream s m Char => ParsecT s u m Char
alphaNumOrUnderscore = alphaNum <|> char '_'
