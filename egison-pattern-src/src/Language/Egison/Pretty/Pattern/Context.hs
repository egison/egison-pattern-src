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
