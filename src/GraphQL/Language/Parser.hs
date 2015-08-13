module GraphQL.Language.Parser where

import           Control.Applicative hiding (many, (<|>))
import           Data.Monoid
import           Text.Parsec
import           Text.Parsec.String

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

withMaybe p q op = do
  resp <- optionMaybe p
  resq <- q
  return $ case resp of
    Nothing -> resq
    Just resp' -> op resp' resq

notString :: String -> Parser ()
notString = notFollowedBy . string

notStrings :: [String] -> Parser ()
notStrings = mapM_ notString

with :: Monad m => m a -> m [a] -> m [a]
with p s = do
  v <- p
  w <- s
  return $ v:w

withMonoid :: (Monad m, Monoid a) =>  m a -> m a -> m a
withMonoid p s = do
  v <- p
  w <- s
  return $ v <> w

options :: [ParsecT s u m a] -> ParsecT s u m a
options = foldr1 (<|>)

-- TODO: Line/Paragraph Separators
lineTerminator :: Parser Char
lineTerminator = oneOf "\r\n"

comma :: Parser Char
comma = char ','

comment :: Parser String
comment = char '#' *> many (noneOf "\r\n")

punctuator :: Parser Punctuator
punctuator = options $ map (uncurry bind) bindings
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
name = oneOf alpha `with` many (oneOf alphaNumeric)
  where alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_"
        alphaNumeric = alpha ++ ['0'..'9']

intValue :: Parser Int
intValue = read <$> withMaybe (char '-') (many1 digit) (:)

floatValue :: Parser Float
floatValue = read <$> options
  [ try (integerPart `withMonoid` exponentPart)
  , try (integerPart `withMonoid` fractionalPart `withMonoid` exponentPart)
  , integerPart `withMonoid` fractionalPart
  ]
  where integerPart = withMaybe (char '-') (many1 digit) (:)
        exponentIndicator = oneOf "eE"
        sign = oneOf "+-"
        fractionalPart = char '.' `with` many1 digit
        exponentPart = exponentIndicator `with` withMaybe sign integerPart (:)

boolValue :: Parser Bool
boolValue = options [string "true" *> pure True, string "false" *> pure False]

data CharValue =
    CV_Char Char
  | CV_EscapedChar Char
  | CV_EscapedUnicode String
    deriving (Show, Eq)

-- | Useful for showing
condense :: StringValue -> String
condense (CV_Char c:xs) = c:condense xs
condense (CV_EscapedChar c:xs) = '\\':c:condense xs
condense (CV_EscapedUnicode str:xs) = '\\':str ++ condense xs
condense [] = []

type StringValue = [CharValue]

stringValue :: Parser StringValue
stringValue = char '"' *> many stringCharacter <* char '"'
  where stringCharacter = try escapedUnicode
                      <|> try escapedCharacter
                      <|> CV_Char <$> noneOf "\"\\\n\r"
        unicode = ['0'..'9'] ++ ['A'..'F'] ++ ['a'..'f']
        escapedUnicode = CV_EscapedUnicode
          <$> (char '\\' *> (char 'u' `with` count 4 (oneOf unicode)))
        escapedCharacter = CV_EscapedChar <$> (char '\\' *> oneOf "\\/bfnrt")

-- | Parse an EnumValue
enumValue :: Parser String
enumValue = notStrings ["null", "false", "true"] *> name

-- | Parse a Variable
variable :: Parser String
variable = char '$' *> name

data GraphQLType = NamedType Name
                 | ListType GraphQLType
                 -- | NB. Allows NonNullType (NonNullType ...), but cannot be
                 -- | created in this way via the parser
                 | NonNullType GraphQLType
                  deriving (Show, Eq)

type_ :: Parser GraphQLType
type_ = try nonNullType <|> try listType <|> namedType

namedType ::Parser GraphQLType
namedType = NamedType <$> name

listType :: Parser GraphQLType
listType = ListType <$> between (char '[') (char ']') type_

nonNullType :: Parser GraphQLType
nonNullType = NonNullType <$> (options [listType, namedType] <* char '!')
