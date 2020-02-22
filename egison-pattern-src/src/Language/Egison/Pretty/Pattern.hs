-- |
--
-- Module:      Language.Egison.Pretty.Pattern
-- Description: Pretty printer for Egison patterns
-- Stability:   experimental
--
-- A pretty printer for Egison patterns.

module Language.Egison.Pretty.Pattern
  ( prettyExpr
  -- * Re-exports
  , module X
  )
where

-- re-exports
import           Language.Egison.Pretty.Pattern.Error
                                               as X
                                                ( Error(..) )
import           Language.Egison.Pretty.Pattern.PrintMode
                                               as X
                                                ( ExtPrinter
                                                , PrintMode(..)
                                                , PageMode(..)
                                                , PrintFixity(..)
                                                )
import           Language.Egison.Syntax.Pattern.Fixity
                                               as X
                                                ( Fixity(..)
                                                , Associativity(..)
                                                , Precedence(..)
                                                )

-- main
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text )
import           Control.Monad.Except           ( MonadError )
import           Language.Egison.Pretty.Pattern.Prim
                                                ( Doc
                                                , hsep
                                                , text
                                                , parens
                                                , (<+>)
                                                , renderDoc
                                                )
import           Language.Egison.Pretty.Pattern.External
                                                ( name
                                                , varName
                                                , valueExpr
                                                )
import           Language.Egison.Pretty.Pattern.Print
                                                ( Print
                                                , runPrint
                                                , askContext
                                                , withContext
                                                , operatorOf
                                                )
import           Language.Egison.Pretty.Pattern.Context
                                                ( Context(..)
                                                , Side(..)
                                                )
import           Language.Egison.Pretty.Pattern.Operator
                                                ( Operator(..)
                                                , OperatorAssoc(..)
                                                )
import           Language.Egison.Syntax.Pattern ( Expr(..) )


smartParens :: Operator -> Doc -> Print n v e Doc
smartParens opr doc = do
  ctx <- askContext
  if check ctx opr then pure $ parens doc else pure doc
 where
  check World               _ = False
  check ConstructorArgument Operator { associativity = Prefix } = False
  check ConstructorArgument _ = True
  check (Under uPrec side) Operator { precedence, associativity }
    | uPrec > precedence = True
    | uPrec == precedence && not (matching associativity side) = True
    | otherwise          = False
  matching InfixRight RightSide = True
  matching InfixLeft  LeftSide  = True
  matching _          _         = False

infix_ :: Ord n => Operator -> Expr n v e -> Expr n v e -> Print n v e Doc
infix_ opr@Operator { precedence, symbol } e1 e2 = do
  d1 <- withContext (Under precedence LeftSide) $ expr e1
  d2 <- withContext (Under precedence RightSide) $ expr e2
  smartParens opr $ d1 <+> text symbol <+> d2

andOperator :: Operator
andOperator = Operator { precedence    = Precedence 3
                       , associativity = InfixRight
                       , symbol        = "&"
                       }

orOperator :: Operator
orOperator = Operator { precedence    = Precedence 2
                      , associativity = InfixRight
                      , symbol        = "|"
                      }

notOperator :: Operator
notOperator =
  Operator { precedence = Precedence 5, associativity = Prefix, symbol = "!" }

expr :: Ord n => Expr n v e -> Print n v e Doc
expr Wildcard     = pure "_"
expr (Variable v) = do
  dv <- varName v
  pure $ "$" <> dv
expr (Value e) = do
  de <- valueExpr e
  pure $ "#" <> de
expr (Predicate e) = do
  de <- valueExpr e
  pure $ "?" <> de
expr (And e1 e2) = infix_ andOperator e1 e2
expr (Or  e1 e2) = infix_ orOperator e1 e2
expr (Not e    ) = do
  let Operator { precedence } = notOperator
  d <- withContext (Under precedence RightSide) $ expr e
  smartParens notOperator $ "!" <> d
expr (Infix n e1 e2) = do
  fixity <- operatorOf n
  infix_ fixity e1 e2
expr (Pattern n []) = name n
expr (Pattern n es) = do
  dn <- name n
  ds <- withContext ConstructorArgument $ traverse expr es
  pure . parens $ dn <+> hsep ds

-- | Pretty print 'Expr'.
prettyExpr
  :: (MonadError (Error n) m, Ord n) => PrintMode n v e -> Expr n v e -> m Text
prettyExpr mode e = do
  doc <- runPrint (expr e) mode
  pure $ renderDoc mode doc
