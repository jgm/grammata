{-# LANGUAGE OverloadedStrings #-}

import Grammata.Parse (interpretDoc)

import Control.Monad.RWS
import Language.Haskell.Interpreter -- hint
import Grammata.Types
import Data.List (isPrefixOf)
import System.Environment
import System.IO (stderr, hPutStrLn)
import Data.Char (toUpper, toLower)
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
  args <- getArgs
  progname <- getProgName
  let verbosity = if "-v2" `elem` args
                     then 2
                     else if "-v1" `elem` args
                          then 1
                          else 0
  let args' = filter (not . isPrefixOf "-v") args
  let (format,file) =
         case args' of
              [(x:xs),y] -> ((toUpper x:map toLower xs),y)
              _ -> error $ "Usage:  " ++ progname ++
                             "[-v1|-v2] [tex|html|pdf] file"

  doc <- if file == "-"
            then getContents
            else readFile file
  r <- runInterpreter (interpretDoc verbosity doc format)

  case r of
       Left (WontCompile es) -> mapM_ (hPutStrLn stderr . errMsg) es
       Left (NotAllowed e) -> hPutStrLn stderr e
       Left (UnknownError e) -> hPutStrLn stderr e
       Left (GhcException e) -> hPutStrLn stderr e
       Right x  -> do
         render x >>= liftIO . BL.putStr
         liftIO $ BL.putStr "\n"
