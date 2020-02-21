module TestImport
  ( testParseExpr
  , testParseExprSpecialFixities
  -- * Re-exports
  , module X
  )
where

-- re-exports
import           Language.Haskell.Exts.Syntax  as X
                                                ( QName(..)
                                                , Name(..)
                                                , ModuleName(..)
                                                , Exp(..)
                                                )

import           Language.Egison.Parser.Pattern
                                               as X
                                                ( Errors )
import           Language.Egison.Syntax.Pattern
                                               as X

-- main
import           Control.Monad.Except           ( MonadError )

import qualified Language.Haskell.Exts.Parser  as Haskell
                                                ( defaultParseMode )

import           Language.Egison.Parser.Pattern ( Fixity(..)
                                                , Precedence(..)
                                                , Associativity(..)
                                                )

import qualified Language.Egison.Parser.Pattern.Mode.Haskell
                                               as HaskellMode
                                                ( Expr
                                                , parseExpr
                                                , parseExprWithFixities
                                                )


testParseExpr :: MonadError (Errors String) m => String -> m HaskellMode.Expr
testParseExpr = HaskellMode.parseExpr Haskell.defaultParseMode

specialFixities :: [Fixity (QName ())]
specialFixities =
  [ Fixity AssocRight (Precedence 5) (sym "++")
  , Fixity AssocRight (Precedence 5) (sym ":")
  ]
  where sym = UnQual () . Symbol ()

testParseExprSpecialFixities
  :: MonadError (Errors String) m => String -> m HaskellMode.Expr
testParseExprSpecialFixities =
  HaskellMode.parseExprWithFixities Haskell.defaultParseMode specialFixities
