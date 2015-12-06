import Control.Monad
import Language.Haskell.Interpreter -- hint
import Grammata
import qualified Data.Text.IO as T
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment
import Data.Either

main :: IO ()
main = do
  args <- getArgs
  name <- getProgName
  let (format,file) = case args of
                          [x,y] -> (x,y)
                          _ -> error $ "Usage:  " ++ name ++ " [TeX|Html] file"

  doc <- fmap (either error T.unpack) (parse <$> T.readFile file)

  r <- runInterpreter (interpretDoc doc format)
  case r of
       Left err -> putStrLn (show err)
       Right () -> return ()

interpretDoc :: String -> String -> Interpreter ()
interpretDoc doc format = do
  loadModules ["Grammata.hs", "Grammata/Format/" ++ format ++ ".hs"]
  set [languageExtensions := [OverloadedStrings, TemplateHaskell]]
  setImportsQ [("Prelude", Nothing), ("Data.Monoid", Nothing), ("Control.Monad.RWS", Nothing), ("Control.Monad.Identity", Nothing), ("Grammata", Nothing), ("Grammata.Format." ++ format, Nothing), ("Data.String", Nothing), ("Language.Haskell.TH", Nothing)]
  liftIO . T.putStrLn . render =<< interpret doc (as :: Doc Block)
