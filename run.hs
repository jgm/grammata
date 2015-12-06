import Control.Monad
import Language.Haskell.Interpreter -- hint
import Grammata
import qualified Data.Text.IO as T
import Data.List (isPrefixOf)
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
       Left (WontCompile es) -> mapM_ (putStrLn . showCompileError file) es
       Left err -> putStrLn (show err)
       Right r -> liftIO . T.putStrLn . render $ r

showCompileError file e =
  if "<interactive>" `isPrefixOf` e'
     then file ++ drop 13 e'
     else e'
  where e' = errMsg e

interpretDoc :: String -> String -> Interpreter (Doc Block)
interpretDoc doc format = do
  loadModules ["Grammata.hs", "Grammata/Format/" ++ format ++ ".hs"]
  set [languageExtensions := [OverloadedStrings, TemplateHaskell]]
  setImportsQ [("Prelude", Nothing), ("Data.Monoid", Nothing), ("Control.Monad.RWS", Nothing), ("Control.Monad.Identity", Nothing), ("Grammata", Nothing), ("Grammata.Format." ++ format, Nothing), ("Data.String", Nothing), ("Language.Haskell.TH", Nothing)]
  interpret doc (as :: Doc Block)
