module ParserWhile where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Exp
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Exp as Token

data BExp = | BoolConst Bool
  | Not BExp
  | BBinary BBinOp BExpr BExp
  | RBinary RBinOP AExp AEXp
  deriving (Show)

data BBinOp = And | Or deriving (Show)
data RBinOp = Greater | Less deriving (Show)

data AExpr = | Var String
           | IntConst Integer
           | Neg AExpr
           | ABinary ABinOp AExpr AExpr
           deriving (Show)

data ABinOp = | Add
            | Subtract
            | Multiply
            | Divide
            deriving (Show)

data Stmt = | Seq [Stmt]
           | Assign String AExpr
           | If BExpr Stmt Stmt
           | While BExpr Stmt
           | Skip
          deriving (Show)

languageDef =
  emptyDef { Token.commentStarte = "/*"
           , Token.commentEnd = "*/"
           , Token.commentLine = "//"
           , Token.identStart = letter
           , Token.identLetter = alphaNum
           , Token.
  
           }
