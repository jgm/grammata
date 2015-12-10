module Grammata.Format.PDF (doc, module Grammata.Format.TeX) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder (byteString)
import Grammata.Types
import Grammata.Format.TeX hiding (doc)
import qualified Grammata.Format.TeX as TeX
import System.Directory
import System.Process
import System.Exit
import System.IO
import System.IO.Temp
import Control.Monad.RWS
import Data.Monoid
import System.FilePath ((</>))

doc :: Doc IO Block -> Doc IO Block
doc d = liftIO (render (TeX.doc d)) >>=
        liftIO . runxetex >>=
        return . Block . byteString

runxetex :: BL.ByteString -> IO B.ByteString
runxetex b = withSystemTempDirectory "grammata.XXXX" $ \dir -> do
    oldDir <- getCurrentDirectory
    setCurrentDirectory dir
    BL.writeFile "temp.tex" b
    ec <- rawSystem "xetex" ["temp.tex"]
    case ec of
        ExitFailure code -> do
          hPutStrLn stderr $ "Could not create PDF:"
          readFile "temp.log" >>= hPutStrLn stderr
          exitWith $ ExitFailure 1
        ExitSuccess      -> do
          setCurrentDirectory oldDir
          B.readFile (dir </> "temp.pdf")
