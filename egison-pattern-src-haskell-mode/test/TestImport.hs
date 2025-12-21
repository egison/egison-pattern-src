module TestImport
  ( testParseExpr,
    testParseExprSpecialFixities,
    testPrintExpr,

    -- * Re-exports
    module X,
  )
where

-- re-exports

-- main

import Control.Monad.Except (MonadError)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Language.Egison.Parser.Pattern as X
  ( Errors,
  )
import Language.Egison.Parser.Pattern.Mode.Haskell
  ( ParseMode (..),
  )
import Language.Egison.Parser.Pattern.Mode.Haskell qualified as HaskellParseMode
  ( Expr,
    ParseMode (..),
    makeParseFixity,
    parseExpr,
  )
import Language.Egison.Pretty.Pattern (Error)
import Language.Egison.Pretty.Pattern.Mode.Haskell qualified as HaskellPrettyMode
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
import Language.Haskell.Exts.Pretty qualified as Haskell
  ( defaultMode,
    style,
  )
import Language.Haskell.Exts.Syntax as X
  ( Exp,
    ModuleName (..),
    Name (..),
    QName (..),
  )

testParseExpr ::
  (MonadError (Errors String) m) => String -> m HaskellParseMode.Expr
testParseExpr = HaskellParseMode.parseExpr mode
  where
    mode =
      HaskellParseMode.ParseMode
        { haskellMode = Haskell.defaultParseMode,
          fixities = Nothing
        }

specialFixities :: [Fixity (QName ())]
specialFixities =
  [ Fixity AssocRight (Precedence 5) (sym "++"),
    Fixity AssocRight (Precedence 5) (sym ":"),
    Fixity AssocRight (Precedence 5) (qsym ":"),
    Fixity AssocLeft (Precedence 7) (qname "mod")
  ]
  where
    sym = UnQual () . Symbol ()
    qsym = Qual () (ModuleName () "Special") . Symbol ()
    qname = Qual () (ModuleName () "Special") . Ident ()

testParseExprSpecialFixities ::
  (MonadError (Errors String) m) => String -> m HaskellParseMode.Expr
testParseExprSpecialFixities = HaskellParseMode.parseExpr mode
  where
    fs = mapMaybe HaskellParseMode.makeParseFixity specialFixities
    mode =
      HaskellParseMode.ParseMode
        { haskellMode = Haskell.defaultParseMode,
          fixities = Just fs
        }

testPrintExpr ::
  (MonadError (Error (QName ())) m) => HaskellPrettyMode.Expr -> m Text
testPrintExpr =
  HaskellPrettyMode.prettyExprWithFixities
    Haskell.style
    Haskell.defaultMode
    specialFixities
