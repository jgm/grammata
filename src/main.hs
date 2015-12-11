{-# LANGUAGE OverloadedStrings #-}

import Grammata.Parse (interpretDoc)
import Grammata.Util (showInterpreterError)
import Options.Applicative hiding (UnknownError)
import Control.Monad.RWS
import Language.Haskell.Interpreter -- hint
import Grammata.Types
import System.IO (stderr, hPutStrLn)
import Data.Char (toUpper, toLower)
import qualified Data.ByteString.Lazy as BL

data Options = Options
  { verbosity :: Int
  , format    :: String
  , inpFile   :: String
  }

options :: Parser Options
options = Options
  <$> option auto
      ( short 'v'
     <> hidden
     <> metavar "[1|2]"
     <> value 0
     <> help "Verbosity level" )
  <*> argument str (metavar "FORMAT")
  <*> argument str (metavar "FILE")

main :: IO ()
main = execParser options' >>= runWithOptions
  where
    options' = info (helper <*> options)
      (briefDesc <> header "grammata - convert text using macros")

runWithOptions :: Options -> IO ()
runWithOptions opts = do
  let formatFormat [] = []
      formatFormat (x:xs) = toUpper x : map toLower xs

  doc <- if (inpFile opts == "-")
            then getContents
            else readFile (inpFile opts)

  r <- runInterpreter (interpretDoc (verbosity opts) doc
           (formatFormat $ format opts))

  case r of
       Left e -> hPutStrLn stderr (showInterpreterError e)
       Right x  -> do
         render x >>= liftIO . BL.putStr
         liftIO $ BL.putStr "\n"


