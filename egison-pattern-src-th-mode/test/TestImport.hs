module TestImport
  ( testParseExpr,
    testPrintExpr,

    -- * Re-exports
    module X,
  )
where

-- re-exports

-- main

import Control.Monad.Except (MonadError)
import Data.Text (Text)
import Language.Egison.Parser.Pattern as X
  ( Errors,
  )
import Language.Egison.Parser.Pattern.Mode.Haskell.TH
  ( ParseMode (..),
  )
import Language.Egison.Parser.Pattern.Mode.Haskell.TH qualified as THParseMode
  ( Expr,
    ParseMode (..),
    parseExpr,
  )
import Language.Egison.Pretty.Pattern (Error)
import Language.Egison.Pretty.Pattern.Mode.Haskell.TH qualified as THPrintMode
  ( Expr,
    prettyExprWithFixities,
  )
import Language.Egison.Syntax.Pattern as X
import Language.Egison.Syntax.Pattern.Fixity
  ( Associativity (..),
    Fixity (..),
    Precedence (..),
  )
import Language.Haskell.Exts.Parser qualified as Haskell
  ( defaultParseMode,
  )
import Language.Haskell.TH.Syntax as X
  ( Exp (..),
    Lit (..),
    Name (..),
    mkName,
  )

testParseExpr :: (MonadError (Errors String) m) => String -> m THParseMode.Expr
testParseExpr = THParseMode.parseExpr mode
  where
    mode =
      THParseMode.ParseMode
        { haskellMode = Haskell.defaultParseMode,
          fixities = Nothing
        }

specialFixities :: [Fixity Name]
specialFixities =
  [ Fixity AssocRight (Precedence 5) (mkName "++"),
    Fixity AssocRight (Precedence 5) (mkName ":"),
    Fixity AssocRight (Precedence 5) (mkName "Special.:"),
    Fixity AssocLeft (Precedence 7) (mkName "mod"),
    Fixity AssocLeft (Precedence 7) (mkName "Special.mod")
  ]

testPrintExpr :: (MonadError (Error Name) m) => THPrintMode.Expr -> m Text
testPrintExpr = THPrintMode.prettyExprWithFixities specialFixities
