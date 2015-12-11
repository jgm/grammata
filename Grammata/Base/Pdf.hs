module Grammata.Base.Pdf (doc, module Grammata.Base.Latex) where

import Data.ByteString.Builder (byteString)
import Grammata.Types
import Grammata.Base.Latex hiding (doc)
import qualified Grammata.Base.Latex as Tex
import Grammata.Util (runxelatex)
import Control.Monad.RWS

doc :: Doc IO Block -> Doc IO Block
doc d = liftIO (render (Tex.doc d)) >>=
        liftIO . runxelatex >>=
        return . Block . byteString


