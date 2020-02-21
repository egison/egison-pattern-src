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

import qualified Language.Egison.Parser.Pattern.Haskell
                                               as HaskellParser
                                                ( Expr
                                                , parseExpr
                                                , parseExprWithFixities
                                                )


testParseExpr :: MonadError (Errors String) m => String -> m HaskellParser.Expr
testParseExpr = HaskellParser.parseExpr Haskell.defaultParseMode

specialFixities :: [Fixity (QName ()) String]
specialFixities =
  [ Fixity AssocRight (Precedence 5) (chunkParser "++")
  , Fixity AssocRight (Precedence 5) (chunkParser ":")
  ]
 where
  chunkParser chunk content
    | chunk == content = Right (UnQual () (Symbol () chunk))
    | otherwise = Left $ "expected " ++ show chunk ++ ", found " ++ show content

testParseExprSpecialFixities
  :: MonadError (Errors String) m => String -> m HaskellParser.Expr
testParseExprSpecialFixities =
  HaskellParser.parseExprWithFixities Haskell.defaultParseMode specialFixities
