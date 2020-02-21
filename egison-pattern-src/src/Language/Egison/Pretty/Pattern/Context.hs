-- |
--
-- Module:      Language.Egison.Pretty.Pattern.Context
-- Description: Pretty-printing context
-- Stability:   experimental
--
-- A pretty-printing context, utilized to control parentheses in infix expressions

module Language.Egison.Pretty.Pattern.Context
  ( Context(..)
  , Side(..)
  )
where

import           Language.Egison.Syntax.Pattern.Fixity
                                                ( Precedence )


data Context
  = World
  | Under Precedence Side
  | ConstructorArgument

data Side = LeftSide | RightSide
