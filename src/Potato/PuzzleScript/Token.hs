{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Potato.PuzzleScript.Token(
  identifier,
  reserved,
  operator,
  reservedOp,
  charLiteral,
  parens,
  brackets,
  integer,
  natural,
  semi,
  symbol,
  whiteSpace,
  commaSep,
  lexeme,

  betweenNoLexeme,
  bracketsNoLexeme

  --objKnown,
  --objTermKnown,
  --legendRhsExpr,
  --winConditionExpr
) where

import Potato.PuzzleScript.Types
import Potato.PuzzleScript.ParserOutput
import Text.Parsec
import Control.Monad.Identity
import qualified Data.Text as T
import qualified Text.Parsec.Token as Token

languageDef_ :: Token.GenLanguageDef T.Text Output Identity
languageDef_ = Token.LanguageDef
           { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.nestedComments = True
           , Token.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
           , Token.opStart        = Token.opLetter languageDef_
           , Token.identStart     = letter <|> char '_'
           , Token.identLetter    = alphaNum <|> oneOf "_'↶↷"
           -- note that operators are always case sensitive
           , Token.caseSensitive   = False
           , Token.reservedNames   = [

                                     -- object ops
                                      "not"
                                      , "and"
                                      , "or"

                                      -- win condition ops
                                      , "All"
                                      , "Some"
                                      , "No"

                                      -- rule modifiers
                                      , "Late"

                                      -- commands
                                     ]
           , Token.reservedOpNames = [ "=", "|", "->", "+" ]
           }

languageDef = languageDef_ {
  Token.reservedNames = Token.reservedNames languageDef_
    ++ map show allKeyboardInputs
}

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
operator = Token.operator lexer
reservedOp = Token.reservedOp lexer -- parses an operator
charLiteral = Token.charLiteral lexer
parens     = Token.parens     lexer
brackets = Token.brackets lexer
integer    = Token.integer    lexer -- parses an integer
natural = Token.natural lexer
semi       = Token.semi       lexer -- parses a semicolon
symbol = Token.symbol lexer
whiteSpace = Token.whiteSpace lexer -- parses whitespace
commaSep = Token.commaSep lexer
lexeme = Token.lexeme lexer




betweenNoLexeme :: (String, String) -> PotatoParser a -> PotatoParser a
betweenNoLexeme (l,r) p = between (lexeme (string l)) (string r) p

bracketsNoLexeme :: PotatoParser a -> PotatoParser a
bracketsNoLexeme = betweenNoLexeme ("[","]")

--DELETE
{-
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
-- ObjExprKnown
objKnown :: ObjectMap -> PotatoParser Object
objKnown lm = (do
  ident <- identifier
  guard $ Map.member ident lm
  return ident) <?> "known object"

objTermKnown :: ObjectMap -> PotatoParser Expr
objTermKnown lm = objKnown lm >>= return . ObjectExpr

legendRhsExpr :: ObjectMap -> PotatoParser Expr
legendRhsExpr lm = try $ buildExpressionParser legendExprOperators (termParser lm) where
  termParser lm' = parens (legendRhsExpr lm) <|> objTermKnown lm

winConditionExpr :: ObjectMap -> PotatoParser Expr
winConditionExpr lm = (try $ do
  let
    termParser lm' = parens (winConditionExpr lm) <|> objTermKnown lm
  expr <- buildExpressionParser winConditionsExprOperators (termParser lm)
  guard $ isWinCondExprition expr
  return expr) <?> "valid win condition expression"
-}
