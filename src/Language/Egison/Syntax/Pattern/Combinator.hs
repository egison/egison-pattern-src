-- |
--
-- Module:      Language.Egison.Syntax.Pattern.Parser.Combinator
-- Description: Useful combinators to manipulate Egison pattern ASTs
-- Stability:   experimental
--
-- Useful combinators to manipulate Egison pattern ASTs.

module Language.Egison.Syntax.Pattern.Combinator
  ( unAnnotate
  )
where

import           Data.Functor.Foldable          ( cata
                                                , embed
                                                )
import           Control.Comonad.Cofree         ( Cofree )
import           Control.Comonad.Trans.Cofree   ( CofreeF(..) )

import           Language.Egison.Syntax.Pattern.Expr
                                                ( Expr )
import           Language.Egison.Syntax.Pattern.Base
                                                ( ExprF )


-- | Unwrap annotations from AST.
unAnnotate :: Cofree (ExprF n e) a -> Expr n e
unAnnotate = cata go where go (_ :< x) = embed x
