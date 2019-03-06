
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

  objConstKnown,
  objTermKnown,
  legendRhsExpr
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
           , Token.reservedOpNames = ["=", "not", "all", "some", "no", "and", "or", "not", "arrow"
                                     ]
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
opNot = Prefix (reservedOp "not" >> return (ObjUn Not))
opAll = Prefix (reservedOp "all" >> return (ObjUn All))
opSome = Prefix (reservedOp "some" >> return (ObjUn Some))
opNo = Prefix (reservedOp "no" >> return (ObjUn No             ))
opAnd = Infix  (reservedOp "and" >> return (ObjBin And     )) AssocLeft
opOr = Infix  (reservedOp "or"  >> return (ObjBin Or      )) AssocLeft
opArrow = Infix  (reservedOp "arrow"  >> return (ObjBin Arrow      )) AssocNone
opOn = Infix  (reservedOp "on"  >> return (ObjBin On      )) AssocNone

legendExprOperators = [[opAnd, opOr]]
winConditionsExprOperators = [[opNot, opAll, opSome], [opOn]]

bOperators = [ [opNot]
             , [opAnd, opOr]
             ]

-- TODO rename all these to use known as prefix
-- objConstKnown
objConstKnown :: ObjectMap -> PotatoParser Object
objConstKnown lm = (do
  ident <- identifier
  guard $ Map.member ident lm
  return ident) <?> "known object"

objTermKnown :: ObjectMap -> PotatoParser ObjExpr
objTermKnown lm = objConstKnown lm >>= return . ObjConst

legendRhsExpr :: ObjectMap -> PotatoParser ObjExpr
legendRhsExpr lm = try $ buildExpressionParser legendExprOperators (termParser lm) where
  termParser lm' = parens (legendRhsExpr lm) <|> objTermKnown lm

winConditionExpr :: ObjectMap -> PotatoParser ObjExpr
winConditionExpr lm = (try $ do
  let
    termParser lm' = winConditionExpr lm <|> objTermKnown lm
  expr <- buildExpressionParser winConditionsExprOperators (termParser lm)
  guard $ isWinConditionExpr expr
  return expr) <?> "valid win condition expression"
