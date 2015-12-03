import Control.Monad
import Language.Haskell.Interpreter -- hint
import Grammata
import qualified Data.Text.IO as T
import System.Environment

main :: IO ()
main = do
  format:_ <- getArgs
  doc <- readFile "test.gram.hs"
  r <- runInterpreter (interpretDoc doc format)
  case r of
       Left err -> putStrLn (show err)
       Right () -> return ()

interpretDoc :: String -> String -> Interpreter ()
interpretDoc doc format = do
  loadModules ["Grammata.hs"]
  set [languageExtensions := [OverloadedStrings]]
  setImportsQ [("Prelude", Nothing), ("Data.Monoid", Nothing), ("Grammata", Nothing), ("Data.String", Nothing)]
  case format of
       "html"  -> liftIO . T.putStrLn . body =<< interpret doc (as :: Doc Block Html)
       "tex"   -> liftIO . T.putStrLn . body =<< interpret doc (as :: Doc Block TeX)
       _       -> error "Unknown format"
