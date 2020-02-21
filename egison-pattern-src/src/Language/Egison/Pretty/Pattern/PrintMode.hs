module Language.Egison.Pretty.Pattern.PrintMode
  ( ExtPrinter
  , PrintFixity(..)
  , PrintMode(..)
  , PageMode(..)
  , Fixity(..)
  )
where

import           Data.Text                      ( Text )

import           Language.Egison.Syntax.Pattern.Fixity
                                                ( Fixity(..) )


type ExtPrinter a = a -> Text

data PrintFixity n =
  PrintFixity { fixity  :: Fixity n
              , printed :: Text
              }

data PageMode =
  PageMode { lineLength :: Int
           , ribbonsPerLine :: Double
           }

-- | Parser configuration.
data PrintMode n v e
  = PrintMode { fixities         :: [PrintFixity n]
              , varNamePrinter   :: ExtPrinter v
              , namePrinter      :: ExtPrinter n
              , valueExprPrinter :: ExtPrinter e
              , pageMode         :: Maybe PageMode
              }
