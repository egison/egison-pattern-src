-- |
--
-- Module:      Language.Egison.Syntax.Pattern.Parser.Combinator
-- Description: Useful combinators to manipulate Egison pattern ASTs
-- Stability:   experimental
--
-- Useful combinators to manipulate Egison pattern ASTs.

module Language.Egison.Syntax.Pattern.Combinator
  ( unAnnotate
  , foldExpr
  , mapName
  , mapValueExpr
  , variables
  )
where

import           Control.Applicative            ( Alternative(..) )
import           Control.Applicative.Combinators
                                                ( choice )
import           Data.Functor.Foldable          ( cata
                                                , embed
                                                )
import           Control.Comonad.Cofree         ( Cofree )
import           Control.Comonad.Trans.Cofree   ( CofreeF(..) )

import           Language.Egison.Syntax.Pattern.Expr
                                                ( Expr(..) )
import           Language.Egison.Syntax.Pattern.Base
                                                ( ExprF(..) )


-- | Unwrap annotations from AST.
unAnnotate :: Cofree (ExprF n e) a -> Expr n e
unAnnotate = cata go where go (_ :< x) = embed x

-- | fold an expr.
--
-- Note that this is just a type specialization of 'cata'.
foldExpr :: (ExprF n e a -> a) -> Expr n e -> a
foldExpr = cata

-- TODO: Implement 'mapName' and 'mapValueExpr' by adding newtype wrapper for them and making them instances of 'MonoFunctor'

-- | Map over @n@ in @Expr n e@.
mapName :: (n -> n') -> Expr n e -> Expr n' e
mapName f = cata go
 where
  go (VariableF n  ) = Variable (f n)
  go (InfixF n a b ) = Infix (f n) a b
  go (PatternF n ps) = Pattern (f n) ps
  -- TODO: omit these verbose matches
  go WildcardF       = Wildcard
  go (ValueF     e)  = Value e
  go (PredicateF e)  = Predicate e
  go (AndF p1 p2  )  = And p1 p2
  go (OrF  p1 p2  )  = Or p1 p2
  go (NotF p1     )  = Not p1

-- | Map over @e@ in @Expr n e@.
mapValueExpr :: (e -> e') -> Expr n e -> Expr n e'
mapValueExpr f = cata go
 where
  go (ValueF     e)   = Value (f e)
  go (PredicateF e)   = Predicate (f e)
  -- TODO: omit these verbose matches
  go WildcardF        = Wildcard
  go (VariableF n   ) = Variable n
  go (InfixF n p1 p2) = Infix n p1 p2
  go (PatternF n  ps) = Pattern n ps
  go (AndF     p1 p2) = And p1 p2
  go (OrF      p1 p2) = Or p1 p2
  go (NotF p1       ) = Not p1

-- | List pattern variables in a pattern.
variables :: Alternative f => Expr n e -> f n
variables = cata go
 where
  go (VariableF n  ) = pure n
  go (InfixF n a b ) = pure n <|> a <|> b
  go (PatternF n ps) = pure n <|> choice ps
  go _               = empty
