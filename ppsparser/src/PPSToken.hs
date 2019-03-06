
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
           , Token.reservedOpNames = ["+", "-", "/", "="
                                     , "<", ">", "and", "or", "not"
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


opNot = Prefix (reservedOp "not" >> return (ObjUn Not             ))
opAnd = Infix  (reservedOp "and" >> return (ObjBin And     )) AssocLeft
opOr = Infix  (reservedOp "or"  >> return (ObjBin Or      )) AssocLeft

legendExprOperators = [[opAnd, opOr]]

bOperators = [ [opNot]
             , [opAnd, opOr]
             ]

-- TODO rename all these to use known as prefix
-- objConstKnown
objConstKnown :: ObjectMap -> PotatoParser Object
objConstKnown lm = (do
  ident <- identifier
  guard $ Map.member ident lm
  return ident) <?> "valid member"

objTermKnown :: ObjectMap -> PotatoParser ObjExpr
objTermKnown lm = objConstKnown lm >>= return . ObjConst

legendRhsExpr :: ObjectMap -> PotatoParser ObjExpr
legendRhsExpr lm = try $ buildExpressionParser legendExprOperators (termParser lm) where
  termParser lm' = parens (legendRhsExpr lm) <|> objTermKnown lm
