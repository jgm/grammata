module Grammata.Base.Pdf (doc, module Grammata.Base.Tex) where

import Data.ByteString.Builder (byteString)
import Grammata.Types
import Grammata.Base.Tex hiding (doc)
import qualified Grammata.Base.Tex as Tex
import Grammata.Util (runxetex)
import Control.Monad.RWS

doc :: Doc IO Block -> Doc IO Block
doc d = liftIO (render (Tex.doc d)) >>=
        liftIO . runxetex >>=
        return . Block . byteString


