-- |
--
-- Module:      Language.Egison.Pretty.Pattern.Prim
-- Description: Primitive printers
-- Stability:   experimental
--
-- Primitive printers.
--
-- Note that all dependencies on printer library are in this module.
module Language.Egison.Pretty.Pattern.Prim
  ( Doc,
    renderDoc,
    text,

    -- * Re-exports
    module X,
  )
where

-- re-exports

-- main

import Data.Text (Text)
import Data.Void (Void)
import Language.Egison.Pretty.Pattern.PrintMode
  ( PageMode (..),
    PrintMode (..),
  )
import Prettyprinter as X
  ( hsep,
    list,
    parens,
    tupled,
    (<+>),
  )
import Prettyprinter qualified as PP
  ( Doc,
    LayoutOptions (..),
    PageWidth (..),
    layoutPretty,
    pretty,
  )
import Prettyprinter.Render.Text qualified as PP
  ( renderStrict,
  )

type Doc = PP.Doc Void

text :: Text -> Doc
text = PP.pretty

renderDoc :: PrintMode n v e -> Doc -> Text
renderDoc PrintMode {pageMode} doc = PP.renderStrict stream
  where
    stream = PP.layoutPretty layoutOptions doc
    layoutOptions = PP.LayoutOptions {PP.layoutPageWidth}
    layoutPageWidth = case pageMode of
      Just PageMode {lineLength, ribbonsPerLine} ->
        PP.AvailablePerLine lineLength ribbonsPerLine
      Nothing -> PP.Unbounded
