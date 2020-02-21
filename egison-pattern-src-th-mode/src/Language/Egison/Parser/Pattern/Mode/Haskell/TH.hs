-- |
--
-- Module:      Language.Egison.Parser.Pattern.Mode.Haskell
-- Description: A parser for Egison pattern expressions to use with Template Haskell
-- Stability:   experimental
--
-- A parser for Egison pattern expressions to use with Template Haskell.

module Language.Egison.Parser.Pattern.Mode.Haskell.TH
  (
  -- * Parsers
    Expr
  , parseExpr
  , parseExprWithFixities
  -- * Converting 'Expr'
  , toTH
  )
where

import           Control.Monad.Except           ( MonadError )

import qualified Language.Haskell.TH.Syntax    as TH
                                                ( Name
                                                , Exp
                                                )
import qualified Language.Haskell.Meta.Syntax.Translate
                                               as TH
                                                ( toExp
                                                , toName
                                                )
import qualified Language.Haskell.Exts.Syntax  as Haskell
                                                ( QName
                                                , Name
                                                , Exp
                                                )
import qualified Language.Haskell.Exts.Parser  as Haskell
                                                ( ParseMode )

import qualified Language.Egison.Syntax.Pattern
                                               as Egison
                                                ( Expr )
import           Language.Egison.Syntax.Pattern ( mapValueExpr
                                                , mapVarName
                                                , mapName
                                                )
import           Language.Egison.Parser.Pattern ( Errors )
import qualified Language.Egison.Parser.Pattern.Mode.Haskell
                                               as HaskellMode
                                                ( ParseFixity
                                                , parseExpr
                                                , parseExprWithFixities
                                                )


-- | A type synonym of 'Egison.Expr' to be used with Template Haskell.
type Expr = Egison.Expr TH.Name TH.Name TH.Exp

-- | Convert 'Egison.Expr' with @haskell-src-exts@ AST into 'Expr' with TemplateHaskell AST
toTH
  :: Egison.Expr (Haskell.QName l1) (Haskell.Name l2) (Haskell.Exp l3) -> Expr
toTH = mapValueExpr TH.toExp . mapVarName TH.toName . mapName TH.toName

-- | Parse 'Expr' using 'Haskell.ParseMode' from @haskell-src-exts@.
parseExpr
  :: MonadError (Errors String) m => Haskell.ParseMode -> String -> m Expr
parseExpr mode = fmap toTH . HaskellMode.parseExpr mode

-- | Parse 'Expr' using 'Haskell.ParseMode' from @haskell-src-exts@, while supplying an explicit list of 'HaskellMode.Fixity'.
-- Note that fixities obtained from 'Haskell.ParseMode' is ignored here.
parseExprWithFixities
  :: MonadError (Errors String) m
  => Haskell.ParseMode
  -> [HaskellMode.ParseFixity]
  -> String
  -> m Expr
parseExprWithFixities mode fixities =
  fmap toTH . HaskellMode.parseExprWithFixities mode fixities
