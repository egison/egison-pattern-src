-- |
--
-- Module:      Language.Egison.Syntax.Pattern.Parser
-- Description: A parser for Egison patterns
-- Stability:   experimental
--
-- A parser for Egison patterns.

module Language.Egison.Syntax.Pattern.Parser
  ( parseExpr
  )
where

import           Control.Monad.Fail             ( MonadFail )
import           Control.Applicative            ( (<|>) )
import           Control.Monad.Combinators      ( many )

import           Language.Egison.Syntax.Pattern.Parser.Prim
                                                ( Parse
                                                , Source
                                                , runParse
                                                , ParseMode
                                                , lexeme
                                                , name
                                                , valueExpr
                                                , try
                                                , (<?>)
                                                )
import           Language.Egison.Syntax.Pattern.Parser.Combinator
                                                ( token
                                                , parens
                                                )
import           Language.Egison.Syntax.Pattern.Parser.Expr
                                                ( exprParser
                                                , Precedence(..)
                                                , Operator(..)
                                                )
import qualified Language.Egison.Syntax.Pattern.Parser.Token
                                               as Token
                                                ( underscore
                                                , hash
                                                , question
                                                , exclamation
                                                , and
                                                , vertical
                                                , dollar
                                                )
import           Language.Egison.Syntax.Pattern.Expr
                                                ( Expr(..) )


primInfixes :: Source s => [(Precedence, [Operator (Parse n e s) (Expr n e)])]
primInfixes =
  [ (Precedence 5, [Prefix (Not <$ token Token.exclamation)])
  , (Precedence 3, [InfixR (And <$ token Token.and)])
  , (Precedence 2, [InfixR (Or <$ token Token.vertical)])
  ]

wildcard :: Source s => Parse n e s (Expr n e)
wildcard = Wildcard <$ token Token.underscore

variable :: Source s => Parse n e s (Expr n e)
variable = do
  token Token.dollar
  n <- lexeme name
  pure $ Variable n

value :: Source s => Parse n e s (Expr n e)
value = do
  token Token.hash
  e <- lexeme valueExpr
  pure $ Value e

predicate :: Source s => Parse n e s (Expr n e)
predicate = do
  token Token.question
  e <- lexeme valueExpr
  pure $ Predicate e

constr :: Source s => Parse n e s (Expr n e)
constr = parens $ do
  n  <- lexeme name
  es <- many expr
  pure $ Pattern n es

atom :: Source s => Parse n e s (Expr n e)
atom =
  try (parens expr)
    <|> wildcard
    <|> variable
    <|> value
    <|> constr
    <|> predicate
    <?> "atomic pattern"

expr :: Source s => Parse n e s (Expr n e)
expr = exprParser primInfixes atom

-- | A parser for 'Expr'.
parseExpr :: (Source s, MonadFail m) => ParseMode n e s -> s -> m (Expr n e)
parseExpr = runParse expr
