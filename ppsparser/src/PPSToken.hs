
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
  objExprFromObjectMap,
  objExprAndOnlyFromObjectMap
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


bAnd = Infix  (reservedOp "and" >> return (ObjBin And     )) AssocLeft
bOperators = [ [Prefix (reservedOp "not" >> return (Not             ))          ]
             , [bAnd,
                Infix  (reservedOp "or"  >> return (ObjBin Or      )) AssocLeft]
             ]

bTerm :: PotatoParser ObjExpr
bTerm =  parens objExpr <|> (identifier >>= return . ObjConst)

objExpr :: PotatoParser ObjExpr
objExpr = buildExpressionParser bOperators bTerm

objExprAndOnly :: PotatoParser ObjExpr
objExprAndOnly = buildExpressionParser [[bAnd]] bTerm



bTermFromObjectMap :: ObjectMap -> PotatoParser ObjExpr
bTermFromObjectMap lm =  parens objExpr <|>
  (do
    ident <- identifier
    guard $ Map.member ident lm
    return . ObjConst $ ident) <?>
  "valid member"

objExprFromObjectMap :: ObjectMap -> PotatoParser ObjExpr
objExprFromObjectMap lm = try $ buildExpressionParser bOperators (bTermFromObjectMap lm)

objExprAndOnlyFromObjectMap ::ObjectMap -> PotatoParser ObjExpr
objExprAndOnlyFromObjectMap lm = try $ buildExpressionParser [[bAnd]] (bTermFromObjectMap lm)
