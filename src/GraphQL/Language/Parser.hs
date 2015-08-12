module GraphQL.Language.Parser where

import           Control.Applicative           hiding (many, (<|>))
import           Text.ParserCombinators.Parsec

data Token =
    T_Punctuator
  | T_Name
  | T_IntValue
  | T_FloatValue
  | T_StringValue
    deriving (Show, Eq)

data Punctuator =
    P_Bang
  | P_Dollar
  | P_OpenParen
  | P_ClosedParen
  | P_Ellipsis
  | P_Colon
  | P_Equals
  | P_At
  | P_OpenSquareBracket
  | P_ClosedSquareBracket
  | P_OpenBracket
  | P_ClosedBracket
    deriving (Show, Eq)

type Name = String

-- TODO: Line/Paragraph Separators
lineTerminator :: Parser Char
lineTerminator = oneOf "\r\n"

comma :: Parser Char
comma = char ','

comment :: Parser String
comment = char '#' *> many (noneOf "\r\n")

punctuator :: Parser Punctuator
punctuator = foldr1 (<|>) $ map (uncurry bind) bindings
  where bind s p = string s *> pure p
        bindings =
          [ ("!", P_Bang)
          , ("$", P_Dollar)
          , ("(", P_OpenParen)
          , (")", P_ClosedParen)
          , ("...", P_Ellipsis)
          , ("@", P_At)
          , ("[", P_OpenSquareBracket)
          , ("]", P_ClosedSquareBracket)
          , ("{", P_OpenBracket)
          , ("}", P_ClosedBracket)
          ]

name :: Parser Name
name = do { h <- oneOf alpha; t <- many (oneOf alphaNumeric); return (h:t); }
  where alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_"
        alphaNumeric = alpha ++ ['0'..'9']
