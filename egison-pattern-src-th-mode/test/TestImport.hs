module TestImport
  ( testParseExpr
  , testPrintExpr
  -- * Re-exports
  , module X
  )
where

-- re-exports
import           Language.Haskell.TH.Syntax    as X
                                                ( Exp(..)
                                                , Name(..)
                                                , Lit(..)
                                                , mkName
                                                )

import           Language.Egison.Parser.Pattern
                                               as X
                                                ( Errors )
import           Language.Egison.Syntax.Pattern
                                               as X

-- main
import           Data.Text                      ( Text )
import           Control.Monad.Except           ( MonadError )

import qualified Language.Haskell.Exts.Parser  as Haskell
                                                ( defaultParseMode )

import           Language.Egison.Syntax.Pattern.Fixity
                                                ( Fixity(..)
                                                , Precedence(..)
                                                , Associativity(..)
                                                )
import qualified Language.Egison.Parser.Pattern.Mode.Haskell.TH
                                               as THParseMode
                                                ( Expr
                                                , parseExpr
                                                )
import           Language.Egison.Pretty.Pattern ( Error )
import qualified Language.Egison.Pretty.Pattern.Mode.Haskell.TH
                                               as THPrintMode
                                                ( Expr
                                                , prettyExprWithFixities
                                                )


testParseExpr :: MonadError (Errors String) m => String -> m THParseMode.Expr
testParseExpr = THParseMode.parseExpr Haskell.defaultParseMode


specialFixities :: [Fixity Name]
specialFixities =
  [ Fixity AssocRight (Precedence 5) (mkName "++")
  , Fixity AssocRight (Precedence 5) (mkName ":")
  , Fixity AssocRight (Precedence 5) (mkName "Special.:")
  , Fixity AssocLeft  (Precedence 7) (mkName "mod")
  , Fixity AssocLeft  (Precedence 7) (mkName "Special.mod")
  ]

testPrintExpr :: MonadError (Error Name) m => THPrintMode.Expr -> m Text
testPrintExpr = THPrintMode.prettyExprWithFixities specialFixities
