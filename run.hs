import Control.Monad
import Language.Haskell.Interpreter -- hint
import Grammata
import qualified Data.Text.IO as T
import System.Environment
--
import Control.Monad
import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Data.List.Split
import Data.Typeable

-- introspect :: String -> [String]
-- introspect s = splitOn " -> " $ show (Data.Typeable.typeOf $(varE (mkName s)))
--

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
  loadModules ["Grammata.hs", "Grammata/Format/" ++ format ++ ".hs"]
  set [languageExtensions := [OverloadedStrings, TemplateHaskell]]
  setImportsQ [("Prelude", Nothing), ("Data.Monoid", Nothing), ("Control.Monad.RWS", Nothing), ("Control.Monad.Identity", Nothing), ("Grammata", Nothing), ("Grammata.Format." ++ format, Nothing), ("Data.String", Nothing)]
  liftIO . T.putStrLn . render =<< interpret doc (as :: Doc Block)
