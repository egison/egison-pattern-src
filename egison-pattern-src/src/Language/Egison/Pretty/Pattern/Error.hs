module Language.Egison.Pretty.Pattern.Error
  ( Error(..)
  )
where


newtype Error n = UnknownInfixOperator n
