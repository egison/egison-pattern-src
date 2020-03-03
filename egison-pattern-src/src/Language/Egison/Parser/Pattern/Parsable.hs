{-# LANGUAGE AllowAmbiguousTypes #-}

module Language.Egison.Parser.Pattern.Parsable
  ( Parsable(..)
  )
where

import           Control.Monad                  ( unless )
import           Control.Monad.Except           ( MonadError(..) )
import           Data.Bifunctor                 ( first )
import           Control.Comonad.Cofree         ( Cofree )
import           Control.Comonad.Trans.Cofree   ( CofreeF(..) )
import           Data.Functor.Foldable          ( Base
                                                , Recursive(..)
                                                , Corecursive(..)
                                                )

import           Language.Egison.Parser.Pattern.Prim.Location
                                                ( Location )
import           Language.Egison.Parser.Pattern.Prim.Source
                                                ( Source(..) )
import           Language.Egison.Parser.Pattern.Prim.Error
                                                ( Errors )
import qualified Language.Egison.Parser.Pattern.Prim.Error
                                               as Error
                                                ( Error(..) )


unAnnotate :: (Recursive x, Corecursive x) => Cofree (Base x) a -> x
unAnnotate = cata go where go (_ :< x) = embed x

class Source s => Parsable a s mode where
  parse :: MonadError (Errors s) m => mode -> s -> m a
  parseWithLocation :: MonadError (Errors s) m => mode -> s -> m (Cofree (Base a) Location)
  parseNonGreedy :: MonadError (Errors s) m => mode -> s -> m (a, s)
  parseNonGreedyWithLocation :: MonadError (Errors s) m => mode -> s -> m (Cofree (Base a) Location, s)

  parseWithLocation mode s = do
    (a, rest) <- parseNonGreedyWithLocation @a mode s
    unless (eof rest) . throwError . pure . Error.UnexpectedEndOfFile $ tokens rest
    pure a

  default parseNonGreedy :: (Recursive a, Corecursive a, MonadError (Errors s) m) => mode -> s -> m (a, s)
  parseNonGreedy mode = fmap (first unAnnotate) . parseNonGreedyWithLocation @a mode

  default parse :: (Recursive a, Corecursive a, MonadError (Errors s) m) => mode -> s -> m a
  parse mode = fmap unAnnotate . parseWithLocation @a mode
