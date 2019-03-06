
module PPSToken(
  identifier,
  reserved,
  operator,
  reservedOp,
  charLiteral,
  parens,
  integer,
  semi,
  whiteSpace,
  commaSep,

  objKnown,
  objTermKnown,
  legendRhsExpr,
  winConditionExpr
) where

import PPSTypes
import Text.Parsec
import Control.Monad.Identity
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Text.Parsec.Token as Token
import Text.Parsec.Expr


emptyDef :: Token.GenLanguageDef T.Text Output Identity
emptyDef = Token.LanguageDef
           { Token.commentStart   = ""
           , Token.commentEnd     = ""
           , Token.commentLine    = ""
           , Token.nestedComments = True
           , Token.identStart     = letter <|> char '_'
           , Token.identLetter    = alphaNum <|> oneOf "_'"
           , Token.opStart        = Token.opLetter emptyDef
           , Token.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
           , Token.reservedOpNames= []
           , Token.reservedNames  = []
           , Token.caseSensitive  = True
           }


languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = [ "if"
                                     , "then"
                                     , "else"
                                     , "while"
                                     , "do"
                                     , "skip"
                                     , "true"
                                     , "false"
                                     , "not"
                                     , "and"
                                     , "or"
                                     ]
           , Token.reservedOpNames = ["=", "not", "All", "Some", "No", "and", "or", "not", "arrow"
                                     ]
           --, Token.caseSensitive   = False
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
operator = Token.operator lexer
reservedOp = Token.reservedOp lexer -- parses an operator
charLiteral = Token.charLiteral lexer
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace
commaSep = Token.commaSep lexer


-- in order of precedence
opAll = Prefix (reservedOp "All" >> return (UnExpr All))
opSome = Prefix (reservedOp "Some" >> return (UnExpr Some))
opNo = Prefix (reservedOp "No" >> return (UnExpr No             ))
opNot = Prefix (reservedOp "not" >> return (UnExpr Not))
opAnd = Infix  (reservedOp "and" >> return (BinExpr And     )) AssocLeft
opOr = Infix  (reservedOp "or"  >> return (BinExpr Or      )) AssocLeft
opArrow = Infix  (reservedOp "arrow"  >> return (BinExpr Arrow      )) AssocNone
opOn = Infix  (reservedOp "on"  >> return (BinExpr On      )) AssocNone

legendExprOperators = [[opAnd, opOr]]
winConditionsExprOperators = [[opNo, opAll, opSome], [opOn]]


-- TODO rename all these to use known as prefix
-- ConstExprKnown
objKnown :: ObjectMap -> PotatoParser Object
objKnown lm = (do
  ident <- identifier
  guard $ Map.member ident lm
  return ident) <?> "known object"

objTermKnown :: ObjectMap -> PotatoParser Expr
objTermKnown lm = objKnown lm >>= return . ConstExpr

legendRhsExpr :: ObjectMap -> PotatoParser Expr
legendRhsExpr lm = try $ buildExpressionParser legendExprOperators (termParser lm) where
  termParser lm' = parens (legendRhsExpr lm) <|> objTermKnown lm

winConditionExpr :: ObjectMap -> PotatoParser Expr
winConditionExpr lm = (try $ do
  let
    termParser lm' = parens (winConditionExpr lm) <|> objTermKnown lm
  expr <- buildExpressionParser winConditionsExprOperators (termParser lm)
  guard $ isWinConditionExpr expr
  return expr) <?> "valid win condition expression"
