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


-- | A position in source code.
data Position
  = Position { line   :: Int
             , column :: Int
             }

-- | Location, a range of positions in source code.
data Location
  = Location { begin :: Position
             , end   :: Position
             }

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
