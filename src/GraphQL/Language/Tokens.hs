module GraphQL.Language.Tokens where

data Token =
  -- | Name
    TName
  -- | Document
  | TDocument
  | TOperatorDefinition
  | TVariableDefinition
  | TVariable
  | TSelectionSet
  | TField
  | TArgument
  -- | Fragments
  | TFragmentSpread
  | TInlineFragment
  | TFragmentDefinition
  -- | Values
  | TIntVaue
  | TFloatValue
  | TStringValue
  | TBoolean
  | TEnum
  | TList
  | TObject
  | TObjectField
  -- | Directives
  | TDirective
  -- | Types
  | TNamedType
  | TListType
  | TNonNullType
    deriving (Show, Eq, Enum)
