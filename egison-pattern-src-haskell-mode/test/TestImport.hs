module TestImport
  ( testParseExpr
  , testParseExprSpecialFixities
  , testPrintExpr
  -- * Re-exports
  , module X
  )
where

-- re-exports
import           Language.Haskell.Exts.Syntax  as X
                                                ( QName(..)
                                                , Name(..)
                                                , ModuleName(..)
                                                , Exp
                                                )

import           Language.Egison.Parser.Pattern
                                               as X
                                                ( Errors )
import           Language.Egison.Syntax.Pattern
                                               as X

-- main
import           Data.Maybe                     ( mapMaybe )
import           Data.Text                      ( Text )
import           Control.Monad.Except           ( MonadError )

import qualified Language.Haskell.Exts.Parser  as Haskell
                                                ( defaultParseMode )
import qualified Language.Haskell.Exts.Pretty  as Haskell
                                                ( style
                                                , defaultMode
                                                )

import           Language.Egison.Syntax.Pattern.Fixity
                                                ( Fixity(..)
                                                , Precedence(..)
                                                , Associativity(..)
                                                )
import qualified Language.Egison.Parser.Pattern.Mode.Haskell
                                               as HaskellParseMode
                                                ( Expr
                                                , ParseMode(..)
                                                , makeParseFixity
                                                , parseExpr
                                                )
import           Language.Egison.Parser.Pattern.Mode.Haskell
                                                ( ParseMode(..) )
import           Language.Egison.Pretty.Pattern ( Error )
import qualified Language.Egison.Pretty.Pattern.Mode.Haskell
                                               as HaskellPrettyMode
                                                ( Expr
                                                , prettyExprWithFixities
                                                )


testParseExpr
  :: MonadError (Errors String) m => String -> m HaskellParseMode.Expr
testParseExpr = HaskellParseMode.parseExpr mode
 where
  mode = HaskellParseMode.ParseMode { haskellMode = Haskell.defaultParseMode
                                    , fixities    = Nothing
                                    }

specialFixities :: [Fixity (QName ())]
specialFixities =
  [ Fixity AssocRight (Precedence 5) (sym "++")
  , Fixity AssocRight (Precedence 5) (sym ":")
  , Fixity AssocRight (Precedence 5) (qsym ":")
  , Fixity AssocLeft  (Precedence 7) (qname "mod")
  ]
 where
  sym   = UnQual () . Symbol ()
  qsym  = Qual () (ModuleName () "Special") . Symbol ()
  qname = Qual () (ModuleName () "Special") . Ident ()

testParseExprSpecialFixities
  :: MonadError (Errors String) m => String -> m HaskellParseMode.Expr
testParseExprSpecialFixities = HaskellParseMode.parseExpr mode
 where
  fs   = mapMaybe HaskellParseMode.makeParseFixity specialFixities
  mode = HaskellParseMode.ParseMode { haskellMode = Haskell.defaultParseMode
                                    , fixities    = Just fs
                                    }

testPrintExpr
  :: MonadError (Error (QName ())) m => HaskellPrettyMode.Expr -> m Text
testPrintExpr = HaskellPrettyMode.prettyExprWithFixities Haskell.style
                                                         Haskell.defaultMode
                                                         specialFixities
