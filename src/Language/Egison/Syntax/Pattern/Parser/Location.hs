-- |
--
-- Module:      Language.Egison.Syntax.Pattern.Location
-- Description: A helper module to handle source code locations
-- Stability:   experimental
--
-- A helper module to handle source code locations during parsing.

module Language.Egison.Syntax.Pattern.Parser.Location
  ( Location(..)
  , Position(..)
  , Locate(..)
  )
where

import           GHC.Generics                   ( Generic )
import           Data.Data                      ( Data
                                                , Typeable
                                                )


-- | A position in source code.
data Position
  = Position { line   :: Int
             , column :: Int
             }
  deriving (Show, Eq, Generic, Data, Typeable)

-- | Location, a range of positions in source code.
data Location
  = Location { begin :: Position
             , end   :: Position
             }
  deriving (Show, Eq, Generic, Data, Typeable)

-- | a 'Monad' that scans over a source code.
class Monad m => Locate m where
  getPosition :: m Position
  getLocation :: m a -> m (a, Location)

  getLocation m = do
    begin <- getPosition
    x <- m
    end <- getPosition
    let location = Location { begin, end }
    pure (x, location)
