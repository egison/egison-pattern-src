module Language.Egison.Pretty.Pattern.Operator
  ( Operator(..)
  , OperatorAssoc(..)
  )
where

import           Data.Text                      ( Text )

import           Language.Egison.Syntax.Pattern.Fixity
                                                ( Precedence )


data OperatorAssoc
  = InfixRight
  | InfixLeft
  | InfixNone
  | Prefix

data Operator =
  Operator { associativity :: OperatorAssoc
           , precedence :: Precedence
           , symbol :: Text
           }
