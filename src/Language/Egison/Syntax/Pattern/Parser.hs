-- |
--
-- Module:      Language.Egison.Syntax.Pattern.Parser
-- Description: A parser for Egison patterns
-- Stability:   experimental
--
-- A parser for Egison patterns.

module Language.Egison.Syntax.Pattern.Parser
  ( parseExprWithLocation
  , parseExpr
  -- * Re-exports
  , module X
  )
where

-- re-exports
import           Language.Egison.Syntax.Pattern.Parser.Prim
                                               as X
                                                ( Source
                                                , ParseMode(..)
                                                , Fixity(..)
                                                , Errors
                                                , Error(..)
                                                , ErrorItem(..)
                                                )
import           Language.Egison.Syntax.Pattern.Parser.Expr
                                               as X
                                                ( Precedence(..)
                                                , Associativity(..)
                                                , ExprL
                                                )
import           Language.Egison.Syntax.Pattern.Parser.Location
                                               as X
                                                ( Location(..)
                                                , Position(..)
                                                )
import           Language.Egison.Syntax.Pattern.Parser.Token
                                               as X
                                                ( IsToken(..) )

-- main
import           Control.Monad.Except           ( MonadError )
import           Control.Applicative            ( (<|>) )
import           Control.Monad.Combinators      ( many )
import           Control.Comonad.Cofree         ( unwrap )

import           Language.Egison.Syntax.Pattern.Parser.Prim
                                                ( Parse
                                                , runParse
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
                                                , Table(..)
                                                , initTable
                                                , addInfix
                                                , addPrefix
                                                )
import qualified Language.Egison.Syntax.Pattern.Parser.Associativity
                                               as Assoc
                                                ( Associativity(..) )
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
                                                ( Expr )
import           Language.Egison.Syntax.Pattern.Base
                                                ( ExprF(..) )
import           Language.Egison.Syntax.Pattern.Combinator
                                                ( unAnnotate )


primInfixes
  :: Source s => [(Precedence, Table (Parse n e s) (ExprF n e) (ExprL n e))]
primInfixes =
  [ (Precedence 5, addPrefix (NotF <$ token Token.exclamation) initTable)
  , (Precedence 3, addInfix Assoc.Right (AndF <$ token Token.and) initTable)
  , (Precedence 2, addInfix Assoc.Right (OrF <$ token Token.vertical) initTable)
  ]

wildcard :: Source s => Parse n e s (ExprF n e a)
wildcard = WildcardF <$ token Token.underscore

variable :: Source s => Parse n e s (ExprF n e a)
variable = do
  token Token.dollar
  n <- lexeme name
  pure $ VariableF n

value :: Source s => Parse n e s (ExprF n e a)
value = do
  token Token.hash
  e <- lexeme valueExpr
  pure $ ValueF e

predicate :: Source s => Parse n e s (ExprF n e a)
predicate = do
  token Token.question
  e <- lexeme valueExpr
  pure $ PredicateF e

constr :: Source s => Parse n e s (ExprF n e (ExprL n e))
constr = withArgs <|> withoutArgs
 where
  withArgs = parens $ do
    n  <- lexeme name
    es <- many expr
    pure $ PatternF n es
  withoutArgs = do
    n <- lexeme name
    pure $ PatternF n []

atom :: Source s => Parse n e s (ExprF n e (ExprL n e))
atom =
  try (unwrap <$> parens expr) -- discarding location once
    <|> wildcard
    <|> variable
    <|> value
    <|> constr
    <|> predicate
    <?> "atomic pattern"

expr :: Source s => Parse n e s (ExprL n e)
expr = exprParser primInfixes atom

-- | A parser for 'Expr' with locations annotated.
parseExprWithLocation
  :: (Source s, MonadError (Errors s) m)
  => ParseMode n e s
  -> s
  -> m (ExprL n e)
parseExprWithLocation = runParse expr

-- | A parser for 'Expr'.
parseExpr
  :: (Source s, MonadError (Errors s) m) => ParseMode n e s -> s -> m (Expr n e)
parseExpr m = fmap unAnnotate . runParse expr m
