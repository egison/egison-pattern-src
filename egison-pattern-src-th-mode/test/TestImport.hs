module TestImport
  ( testParseExpr
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
import           Control.Monad.Except           ( MonadError )

import qualified Language.Haskell.Exts.Parser  as Haskell
                                                ( defaultParseMode )

import qualified Language.Egison.Parser.Pattern.Mode.Haskell.TH
                                               as THMode
                                                ( Expr
                                                , parseExpr
                                                )


testParseExpr :: MonadError (Errors String) m => String -> m THMode.Expr
testParseExpr = THMode.parseExpr Haskell.defaultParseMode
