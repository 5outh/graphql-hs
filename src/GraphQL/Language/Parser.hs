module GraphQL.Language.Parser where

import           Control.Applicative hiding (many, (<|>))
import           Text.Parsec

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

data Document =
    D_OperatorDefinition
  | D_FragmentDefinition
    deriving (Show, Eq)

type Name = String

-- TODO: Line/Paragraph Separators
lineTerminator :: Parsec String () Char
lineTerminator = oneOf "\r\n"

comma :: Parsec String () Char
comma = char ','

comment :: Parsec String () String
comment = char '#' *> many (noneOf "\r\n")

punctuator :: Parsec String () Punctuator
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

name :: Parsec String () Name
name = do { h <- oneOf alpha; t <- many (oneOf alphaNumeric); return (h:t); }
  where alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_"
        alphaNumeric = alpha ++ ['0'..'9']

intValue :: Parsec String () Int
intValue = do
  neg <- optionMaybe (char '-')
  i <- many1 digit
  pure $ case neg of
    Nothing -> read i
    Just _ -> negate $ read i

with p s = do
  v <- p
  w <- s
  pure $ v:w

floatValue = undefined
  where integerPart = intValue
        exponentIndicator :: Parsec String () Char
        exponentIndicator = oneOf "eE"
        fractionalPart :: Parsec String () String
        fractionalPart = char '.' `with` many1 digit
