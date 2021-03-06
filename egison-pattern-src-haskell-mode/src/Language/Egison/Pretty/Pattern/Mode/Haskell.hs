-- |
--
-- Module:      Language.Egison.Pretty.Pattern.Mode.Haskell
-- Description: Printer for Egison pattern expressions in Haskell source code
-- Stability:   experimental
--
-- A printer for Egison pattern expressions in Haskell source code.

module Language.Egison.Pretty.Pattern.Mode.Haskell
  (
  -- * Printers
    Expr
  , prettyExpr
  , prettyExprWithFixities
  -- * Converting @haskell-src-exts@'s entities
  , PrintMode
  , PrintFixity
  , Fixity
  , makePageMode
  , makeHaskellMode
  , makePrintFixity
  )
where

import           Data.Char                      ( isUpper )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Control.Monad.Except           ( MonadError )
import           Language.Haskell.Exts.Syntax   ( QName(..)
                                                , QOp(..)
                                                , Exp
                                                , Name(..)
                                                )
import qualified Language.Haskell.Exts.Pretty  as Haskell
                                                ( Style(..)
                                                , PPHsMode
                                                , Pretty
                                                , prettyPrint
                                                , prettyPrintStyleMode
                                                )
import qualified Language.Egison.Syntax.Pattern
                                               as Egison
                                                ( Expr )
import qualified Language.Egison.Pretty.Pattern
                                               as Egison
                                                ( PrintMode(..)
                                                , PrintFixity(..)
                                                , Fixity(..)
                                                , PageMode(..)
                                                , prettyExpr
                                                )
import           Language.Egison.Pretty.Pattern ( Error )


-- | Type synonym of 'Egison.Expr' to be printed with Haskell's source code.
type Expr = Egison.Expr (QName ()) (Name ()) (Exp ())

-- | Type synonym of 'Egison.PrintMode' to print 'Expr'.
type PrintMode = Egison.PrintMode (QName ()) (Name ()) (Exp ())

-- | Type synonym of 'Egison.Fixity' to print 'Expr'.
type Fixity = Egison.Fixity (QName ())

-- | Type synonym of 'Egison.PrintFixity' to print 'Expr'.
type PrintFixity = Egison.PrintFixity (QName ())

-- | Build 'Egison.PageMode' using 'Haskell.Style' from @haskell-src-exts@.
makePageMode :: Haskell.Style -> Egison.PageMode
makePageMode Haskell.Style { Haskell.lineLength, Haskell.ribbonsPerLine } =
  Egison.PageMode { Egison.lineLength
                  , Egison.ribbonsPerLine = realToFrac ribbonsPerLine
                  }

-- | Build 'PrintFixity' using 'Fixity' to print Haskell operators.
makePrintFixity :: Fixity -> PrintFixity
makePrintFixity fixity@(Egison.Fixity _ _ sym) = Egison.PrintFixity
  { Egison.fixity
  , Egison.printed = pack $ printSym sym
  }
 where
  printSym q@(UnQual () name) = printName q name
  printSym q@(Qual () _ name) = printName q name
  printSym (  Special () s  ) = Haskell.prettyPrint s
  printName q name | isCon name = Haskell.prettyPrint $ QConOp () q
                   | otherwise  = Haskell.prettyPrint $ QVarOp () q
  isCon (Ident  () (c   : _)) = isUpper c
  isCon (Symbol () (':' : _)) = True
  isCon _                     = False

-- | Build 'PrintMode' using 'Haskell.Style' and 'Haskell.PPHsMode' from @haskell-src-exts@, and the list of fixities.
makeHaskellMode :: Haskell.Style -> Haskell.PPHsMode -> [Fixity] -> PrintMode
makeHaskellMode style mode fixities = Egison.PrintMode
  { Egison.fixities         = map makePrintFixity fixities
  , Egison.varNamePrinter   = pprint
  , Egison.namePrinter      = pprint
  , Egison.valueExprPrinter = pprint
  , Egison.pageMode         = Just $ makePageMode style
  }
 where
  pprint :: Haskell.Pretty a => a -> Text
  pprint = pack . Haskell.prettyPrintStyleMode style mode

-- | Print 'Expr' using 'Haskell.Style' and 'Haskell.PPHsMode' from @haskell-src-exts@.
prettyExpr
  :: MonadError (Error (QName ())) m
  => Haskell.Style
  -> Haskell.PPHsMode
  -> Expr
  -> m Text
prettyExpr style mode = Egison.prettyExpr (makeHaskellMode style mode [])

-- | Print 'Expr' using 'Haskell.Style' and 'Haskell.PPHsMode' from @haskell-src-exts@, while supplying an explicit list of 'Fixity'.
prettyExprWithFixities
  :: MonadError (Error (QName ())) m
  => Haskell.Style
  -> Haskell.PPHsMode
  -> [Fixity]
  -> Expr
  -> m Text
prettyExprWithFixities style mode fixities =
  Egison.prettyExpr (makeHaskellMode style mode fixities)
