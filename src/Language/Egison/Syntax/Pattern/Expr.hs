-- |
--
-- Module:      Language.Egison.Syntax.Pattern.Base
-- Description: An abstract syntax tree for Egison pattern expression
-- Stability:   experimental
--
-- This module defines an AST (Abstract Syntax Tree) for Egison pattern expression.

module Language.Egison.Syntax.Pattern.Expr
  ( Expr(..)
  )
where

-- | Egison pattern expressions.
-- @n@ is a type for names in patterns, such as them in pattern variables.
-- @e@ is a type for expressions in patterns, such as them in value patterns.
data Expr n e
  -- | Wildcard pattern. Match with everything.
  = Wildcard
  -- | Pattern variable. The matched term is bound to the name.
  | Variable n
  -- | Value pattern. Match with equal terms.
  | Value e
  -- | Predicate pattern. Match with terms that satisfy the given predicate.
  | Predicate e
  -- | And pattern. Match when both patterns matches.
  | And (Expr n e) (Expr n e)
  -- | Or pattern. Match when one of given patterns matches.
  | Or (Expr n e) (Expr n e)
  -- | Not pattern. Match when the given pattern does not match.
  | Not (Expr n e)
  -- | User-defined infix pattern.
  | Infix n (Expr n e) (Expr n e)
  -- | User-defined normal pattern.
  | Pattern n [Expr n e]