-- |
--
-- Module:      Language.Egison.Parser.Pattern.Mode.Haskell
-- Description: Printer for Egison pattern expressions to use with Template Haskell
-- Stability:   experimental
--
-- A printer for Egison pattern expressions to use with Template Haskell.

module Language.Egison.Pretty.Pattern.Mode.Haskell.TH
  (
  -- * Printers
    Expr
  , prettyExpr
  , prettyExprWithFixities
  -- * Converting @template-haskell@'s entities
  , PrintMode
  , PrintFixity
  , Fixity
  , makeTHMode
  , makePrintFixity
  )
where

import           Data.Text                      ( Text
                                                , pack
                                                )
import           Control.Monad.Except           ( MonadError )

import qualified Text.PrettyPrint              as PP
                                                ( render )
import qualified Language.Haskell.TH.Syntax    as TH
                                                ( Name
                                                , Exp
                                                , NameIs(..)
                                                )
import qualified Language.Haskell.TH.PprLib    as TH
                                                ( to_HPJ_Doc
                                                , pprName'
                                                )
import qualified Language.Haskell.TH.Ppr       as TH
                                                ( pprint )

import qualified Language.Egison.Syntax.Pattern
                                               as Egison
                                                ( Expr )
import qualified Language.Egison.Pretty.Pattern
                                               as Egison
                                                ( PrintMode(..)
                                                , Fixity(..)
                                                , PrintFixity(..)
                                                , Error
                                                , prettyExpr
                                                )


-- | Type synonym of 'Egison.Expr' to be printed with Template Haskell.
type Expr = Egison.Expr TH.Name TH.Name TH.Exp

-- | Type synonym of 'Egison.PrintMode' to print 'Expr'.
type PrintMode = Egison.PrintMode TH.Name TH.Name TH.Exp

-- | Type synonym of 'Egison.Fixity' to print 'Expr'.
type Fixity = Egison.Fixity TH.Name

-- | Type synonym of 'Egison.PrintFixity' to print 'Expr'.
type PrintFixity = Egison.PrintFixity TH.Name

-- | Build 'PrintFixity' using 'Fixity' to print Haskell operators.
makePrintFixity :: Fixity -> PrintFixity
makePrintFixity fixity@(Egison.Fixity _ _ sym) = Egison.PrintFixity
  { Egison.fixity
  , Egison.printed = printSym sym
  }
  where printSym s = pack . PP.render . TH.to_HPJ_Doc $ TH.pprName' TH.Infix s

-- | Build 'PrintMode' using the list of fixities.
makeTHMode :: [Fixity] -> PrintMode
makeTHMode fixities = Egison.PrintMode
  { Egison.fixities         = map makePrintFixity fixities
  , Egison.varNamePrinter   = printName
  , Egison.namePrinter      = printName
  , Egison.valueExprPrinter = printValueExpr
  , Egison.pageMode         = Nothing
  }
 where
  printValueExpr = pack . TH.pprint
  printName n = pack . PP.render . TH.to_HPJ_Doc $ TH.pprName' TH.Applied n

-- | Print 'Expr'.
prettyExpr :: MonadError (Egison.Error TH.Name) m => Expr -> m Text
prettyExpr = Egison.prettyExpr $ makeTHMode []

-- | Print 'Expr' with an explicit list of 'Fixity'.
prettyExprWithFixities
  :: MonadError (Egison.Error TH.Name) m => [Fixity] -> Expr -> m Text
prettyExprWithFixities = Egison.prettyExpr . makeTHMode
