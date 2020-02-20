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

import           Language.Egison.Syntax.Pattern.Parser
                                               as X
                                                ( Errors )
import           Language.Egison.Syntax.Pattern
                                               as X

-- main
import           Control.Monad.Except           ( MonadError )

import qualified Language.Haskell.Exts.Parser  as Haskell
                                                ( defaultParseMode )

import qualified Language.Egison.Syntax.Pattern.Parser.Haskell.TH
                                               as THParser
                                                ( Expr
                                                , parseExpr
                                                )


testParseExpr :: MonadError (Errors String) m => String -> m THParser.Expr
testParseExpr = THParser.parseExpr Haskell.defaultParseMode
