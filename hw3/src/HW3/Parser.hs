{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module HW3.Parser where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.ByteString.Internal (packChars)
import Data.Text (pack)
import Data.Void (Void)
import HW3.Base
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (ParseErrorBundle)

type Parser = Parsec Void String

sc :: Parser ()
sc =
  L.space
    space1 -- (2)
    (L.skipLineComment "//") -- (3)
    (L.skipBlockComment "/*" "*/") -- (4)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser pExpr ""

pValue :: Parser HiExpr
pValue = do
  sc
  choice
    [ pOp,
      pValueBytes,
      pList,
      pString,
      pValueNull,
      pValueFunction,
      pValueBool,
      pNumber
    ]

pValueBool :: Parser HiExpr
pValueBool = do
  sc
  bool <-
    choice
      [ True <$ string "true",
        False <$ string "false"
      ]
  return $ HiExprValue (HiValueBool bool)

pValueNull :: Parser HiExpr
pValueNull = do
  sc
  _ <- string "null"
  return $ HiExprValue HiValueNull

pValueBytes :: Parser HiExpr
pValueBytes = do
  sc
  str <- string "[#" >> manyTill L.charLiteral (string "#]")
  return $ HiExprValue (HiValueBytes (packChars str))

pValueFunction :: Parser HiExpr
pValueFunction = do
  sc
  func <-
    choice
      [ HiFunDiv <$ string "div",
        HiFunMul <$ string "mul",
        HiFunAdd <$ string "add",
        HiFunSub <$ string "sub",
        HiFunNot <$ string "not",
        HiFunAnd <$ string "and",
        HiFunOr <$ string "or",
        HiFunLessThan <$ string "less-than",
        HiFunGreaterThan <$ string "greater-than",
        HiFunEquals <$ string "equals",
        HiFunNotLessThan <$ string "not-less-than",
        HiFunNotGreaterThan <$ string "not-greater-than",
        HiFunNotEquals <$ string "not-equals",
        HiFunIf <$ string "if",
        HiFunLength <$ string "length",
        HiFunToUpper <$ string "to-upper",
        HiFunToLower <$ string "to-lower",
        HiFunReverse <$ string "reverse",
        HiFunTrim <$ string "trim",
        HiFunList <$ string "list",
        HiFunRange <$ string "range",
        HiFunFold <$ string "fold",
        HiFunPackBytes <$ string "pack-bytes",
        HiFunUnpackBytes <$ string "unpack-bytes",
        HiFunEncodeUtf8 <$ string "encode-utf8",
        HiFunDecodeUtf8 <$ string "decode-utf8",
        HiFunZip <$ "zip",
        HiFunSerialise <$ "serialize",
        HiFunDeserialise <$ "deserialize"
      ]
  return $ HiExprValue (HiValueFunction func)

pNumber :: Parser HiExpr
pNumber = do
  sc
  number <- L.signed sc $ toRational <$> lexeme L.scientific
  return $ HiExprValue (HiValueNumber number)

pString :: Parser HiExpr
pString = do
  sc
  str <- char '"' >> manyTill L.charLiteral (char '"')
  return $ HiExprValue (HiValueString (pack str))

pList :: Parser HiExpr
pList = do
  sc
  list <- between (char '[') (char ']') pArguments
  return $ HiExprApply (HiExprValue (HiValueFunction HiFunList)) list

pExpr :: Parser HiExpr
pExpr = do
  sc
  expr <- pValue
  prev <- between (char '(') (char ')') (HiExprApply expr <$> pArguments) <|> return expr
  pNextExpr prev

pNextExpr :: HiExpr -> Parser HiExpr
pNextExpr expr = do
  sc
  between (char '(') (char ')') (HiExprApply expr <$> pArguments) <|> return expr

pArguments :: Parser [HiExpr]
pArguments = do
  sc
  sepBy1 pExpr (char ',') <|> return []

pOp :: Parser HiExpr
pOp = do
  sc
  makeExprParser pTerm operatorTable

parens :: Parser a -> Parser a
parens parser = do
  sc
  between (char '(') (char ')') parser

pTerm :: Parser HiExpr
pTerm =
  choice
    [ parens pOp,
      pNumber,
      pString
    ]

operatorTable :: [[Operator Parser HiExpr]]
operatorTable =
  [ [ binary '*' $ createBinaryOp HiFunMul,
      binary '/' $ createBinaryOp HiFunDiv
    ],
    [ binary '+' $ createBinaryOp HiFunAdd,
      binary '-' $ createBinaryOp HiFunSub
    ]
  ]

createBinaryOp :: HiFun -> HiExpr -> HiExpr -> HiExpr
createBinaryOp fun a b = HiExprApply (HiExprValue (HiValueFunction fun)) [a, b]

putArgs :: HiExpr -> HiExpr -> [HiExpr]
putArgs a b = [a, b]

binary :: Char -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
binary name f = InfixL (f <$ char name)
