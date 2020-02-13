{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Egison.Syntax.Pattern.Base
  ( ExprF(..)
  )
where

import           Data.Functor.Foldable.TH       ( makeBaseFunctor )

import           Language.Egison.Syntax.Pattern.Expr
                                                ( Expr(..) )


$(makeBaseFunctor ''Expr)
