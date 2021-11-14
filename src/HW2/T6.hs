{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HW2.T6
  ( Parser (..),
    ParseError (..),
    runP,
    pChar,
    parseError,
    pEof,
    parseExpr,
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..), mfilter, msum, void)
import Data.Char (isDigit, isSpace)
import GHC.Natural (Natural)
import HW2.T1 (Annotated (..), Except (..), mapExcept)
import HW2.T4 (Expr (..), Prim (..))
import HW2.T5 (ExceptState (..))

data ParseError = ErrorAtPos Natural

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

-- | Running parser for line.
runP :: Parser a -> String -> Except ParseError a
runP (P parser) line = mapExcept g (runES parser (0, line))
  where
    g (a :# _) = a

-- | Parse one `Char` from line, and
-- increase state counter.
pChar :: Parser Char
pChar = P $ ES \(pos, s) ->
  case s of
    [] -> Error (ErrorAtPos pos)
    (c : cs) -> Success (c :# (pos + 1, cs))

-- | Raise error in parser Monad.
parseError :: Parser a
parseError = P $ ES \(pos, _) -> Error (ErrorAtPos pos)

instance Alternative Parser where
  empty = parseError
  (P l) <|> (P r) = P $ ES \input ->
    chose (runES l input) (runES r input)
    where
      chose (Error _) a = a
      chose a _ = a

instance MonadPlus Parser where
  mzero = empty
  mplus = (<|>)

-- | Eof parser - raise Error if the line
-- has not ended (at that position).
pEof :: Parser ()
pEof = P $ ES \(pos, s) ->
  case s of
    [] -> Success (() :# (pos, []))
    _ -> Error (ErrorAtPos pos)

-- | White space parser (for skip).
pSpace :: Parser ()
pSpace = void . many $ mfilter isSpace pChar

-- | Satisfy parser.
pSymbol :: Char -> Parser Char
pSymbol c = pSpace >> mfilter (c ==) pChar

-- | Parentheses parser.
parseParentheses :: Parser Expr
parseParentheses = pSymbol '(' >> pExpr <* pSymbol ')'

-- | Digit parser.
pDigit :: Parser String
pDigit = some (mfilter isDigit pChar)

-- | Digit parser to Expr.
parseDigit :: Parser Expr
parseDigit =
  Val . read
    <$> ( pSpace
            >> msum
              [ (\x y -> x ++ ['.'] ++ y)
                  <$> (pDigit <* pSymbol '.')
                  <*> pDigit,
                pDigit
              ]
        )

-- | Parser for highest priority (Digit, Parentheses).
parseDigitOrParentheses :: Parser Expr
parseDigitOrParentheses = msum [parseDigit, parseParentheses]

-- | Parser for lowest priority (+, -).
parseLow :: (Expr -> Expr -> Expr) -> Parser (Expr -> Expr)
parseLow op =
  do
    expr <- parseHigh (\_ y -> y)
    f <-
      msum
        [ pSymbol '+' >> parseLow (\x y -> Op (Add x y)),
          pSymbol '-' >> parseLow (\x y -> Op (Sub x y)),
          pure id
        ]
    return (\x -> f (op x (expr (Val 0))))

-- | Parser for high priority (*, /).
parseHigh :: (Expr -> Expr -> Expr) -> Parser (Expr -> Expr)
parseHigh op =
  do
    expr <- parseDigitOrParentheses
    f <-
      msum
        [ pSymbol '*' >> parseHigh (\x y -> Op (Mul x y)),
          pSymbol '/' >> parseHigh (\x y -> Op (Div x y)),
          pure id
        ]
    return (\x -> f (op x expr))

-- | Parser for calculator without extra symbols.
pExpr :: Parser Expr
pExpr =
  do
    expr <- parseLow (\_ y -> y)
    return (expr (Val 0))

-- | Parse (with possible Error) `String` to Expr.
parseExpr :: String -> Except ParseError Expr
parseExpr = runP (pExpr <* (pSpace >> pEof))
