-- |
--
-- Module:      Language.Egison.Syntax.Pattern
-- Description: Manipulating Egison patterns: abstract syntax, parser, and pretty-printer
-- Stability:   experimental
--
-- This package provides a standalone syntax definition for patterns in [Egison programming language](https://egison.org/).

module Language.Egison.Syntax.Pattern
  ( module X
  )
where

import           Language.Egison.Syntax.Pattern.Expr
                                               as X
import           Language.Egison.Syntax.Pattern.Base
                                               as X
import           Language.Egison.Syntax.Pattern.Combinator
                                               as X

import           Language.Egison.Syntax.Pattern.Parser
                                               as X
