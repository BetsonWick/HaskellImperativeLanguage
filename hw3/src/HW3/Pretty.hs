module HW3.Pretty where

import Data.Scientific(fromRationalRepetendUnlimited)
import Data.Text.Prettyprint.Doc(Doc, unsafeViaShow)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import HW3.Base

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNumber a) = unsafeViaShow $ fromRationalRepetendUnlimited a