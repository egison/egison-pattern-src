-- |
--
-- Module:      Language.Egison.Syntax.Pattern.Parser.Expr
-- Description: A helper module to build a parser for expressions
-- Stability:   experimental
--
-- A helper module to build a parser for expressions.

module Language.Egison.Syntax.Pattern.Parser.Expr
  ( exprParser
  , Operator(..)
  , Precedence(..)
  )
where

import qualified Data.IntMap                   as IntMap
                                                ( toDescList
                                                , insertWith
                                                , fromList
                                                )
import           Control.Monad.Reader           ( MonadReader(..) )
import           Data.Bifunctor                 ( first )

import           Control.Monad.Combinators.Expr ( Operator(..)
                                                , makeExprParser
                                                )

import           Language.Egison.Syntax.Pattern.Parser.Prim
                                                ( Fixity(..)
                                                , ParseMode(..)
                                                , Source
                                                , Parse
                                                , liftP
                                                , lexeme
                                                )
import           Language.Egison.Syntax.Pattern.Expr
                                                ( Expr(Infix) )
import qualified Language.Egison.Syntax.Pattern.Parser.Associativity
                                               as Assoc
                                                ( Associativity(..) )
import           Language.Egison.Syntax.Pattern.Parser.Precedence
                                                ( Precedence(..) )
import qualified Language.Egison.Syntax.Pattern.Parser.Precedence
                                               as Prec
                                                ( toInt )


-- | Build an operator table from primitive operator table and 'ParseMode' context.
-- Note that the behavior is undefined when the supplied 'ParseMode' contains some fixities that conflict with 'primInfixes'
buildOperatorTable
  :: (MonadReader (ParseMode n e s) m, Source s)
  => [(Precedence, [Operator (Parse n e s) (Expr n e)])]
  -> m [[Operator (Parse n e s) (Expr n e)]]
buildOperatorTable primInfixes = do
  ParseMode { fixities } <- ask
  pure . map snd . IntMap.toDescList $ foldr go prim fixities
 where
  go (Fixity assoc prec p) =
    IntMap.insertWith (++) (Prec.toInt prec) [makeOperator assoc p]
  makeOperator assoc p = infixCtor assoc $ Infix <$> lexeme (liftP p)
  infixCtor Assoc.Left  = InfixL
  infixCtor Assoc.Right = InfixR
  infixCtor Assoc.None  = InfixN
  prim = IntMap.fromList $ map (first Prec.toInt) primInfixes

-- | Build an expression parser from an atom parser.
exprParser
  :: Source s
  => [(Precedence, [Operator (Parse n e s) (Expr n e)])]
  -> Parse n e s (Expr n e)
  -> Parse n e s (Expr n e)
exprParser primInfixes atom = do
  ops <- buildOperatorTable primInfixes
  makeExprParser atom ops
