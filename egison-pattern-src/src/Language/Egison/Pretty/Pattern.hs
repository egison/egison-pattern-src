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
import           Control.Monad.Except           ( MonadError(..) )
import           Language.Egison.Pretty.Pattern.Prim
                                                ( Doc
                                                , hsep
                                                , text
                                                , parens
                                                , (<+>)
                                                , renderDoc
                                                )
import           Language.Egison.Pretty.Pattern.Error
                                                ( Error(UnknownInfixOperator) )
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
                                                ( Operator(..) )
import           Language.Egison.Syntax.Pattern ( Expr(..) )


smartParens :: Operator -> Doc -> Print n v e Doc
smartParens opr doc = do
  ctx <- askContext
  if check ctx opr then pure $ parens doc else pure doc
 where
  check World               _          = False
  check ConstructorArgument PrefixOp{} = False
  check ConstructorArgument _          = True
  check (Under uPrec side) InfixOp { precedence, associativity }
    | uPrec > precedence = True
    | uPrec == precedence && not (matching associativity side) = True
    | otherwise          = False
  check (Under uPrec _) PrefixOp { precedence } | uPrec >= precedence = True
                                                | otherwise           = False
  matching AssocRight RightSide = True
  matching AssocLeft  LeftSide  = True
  matching _          _         = False

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
expr (And e1 e2) = do
  d1 <- withContext (Under andPrecedence LeftSide) $ expr e1
  d2 <- withContext (Under andPrecedence RightSide) $ expr e2
  smartParens opr $ d1 <+> "&" <+> d2
 where
  opr = InfixOp { precedence    = andPrecedence
                , associativity = andAssociativity
                , symbol        = "&"
                }
  andPrecedence    = Precedence 3
  andAssociativity = AssocRight
expr (Or e1 e2) = do
  d1 <- withContext (Under orPrecedence LeftSide) $ expr e1
  d2 <- withContext (Under orPrecedence RightSide) $ expr e2
  smartParens opr $ d1 <+> "|" <+> d2
 where
  opr = InfixOp { precedence    = orPrecedence
                , associativity = orAssociativity
                , symbol        = "|"
                }
  orPrecedence    = Precedence 2
  orAssociativity = AssocRight
expr (Not e) = do
  d <- withContext (Under notPrecedence RightSide) $ expr e
  smartParens opr $ "!" <> d
 where
  opr           = PrefixOp { precedence = notPrecedence, symbol = "!" }
  notPrecedence = Precedence 5
expr (Infix n e1 e2) = do
  opr <- operatorOf n
  case opr of
    InfixOp { precedence, symbol } -> do
      d1 <- withContext (Under precedence LeftSide) $ expr e1
      d2 <- withContext (Under precedence RightSide) $ expr e2
      smartParens opr $ d1 <+> text symbol <+> d2
    _ -> throwError $ UnknownInfixOperator n
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
