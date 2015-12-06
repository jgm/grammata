module Grammata.Format.PDF (doc, module Grammata.Format.TeX) where

import Data.ByteString.Builder (byteString)
import Grammata.Types
import Grammata.Format.TeX hiding (doc)
import qualified Grammata.Format.TeX as TeX
import Grammata.Util (runxetex)
import Control.Monad.RWS

doc :: Doc IO Block -> Doc IO Block
doc d = liftIO (render (TeX.doc d)) >>=
        liftIO . runxetex >>=
        return . Block . byteString


