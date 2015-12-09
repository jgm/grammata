import Language.Haskell.Interpreter -- hint
import Data.String
import Grammata.Types
import Grammata.TH
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.List (isPrefixOf)
import System.Environment
import Text.Parsec

main :: IO ()
main = do
  args <- getArgs
  progname <- getProgName
  let (format,file) =
         case args of
              [x,y] -> (x,y)
              _ -> error $ "Usage:  " ++ progname ++ " [TeX|Html] file"

  doc <- readFile file
  r <- runInterpreter (interpretDoc doc format)

  case r of
       Left (WontCompile es) -> mapM_ (putStrLn . showCompileError file) es
       Left err -> putStrLn (show err)
       Right x -> liftIO . T.putStrLn =<< render x

showCompileError :: [Char] -> GhcError -> [Char]
showCompileError file e =
  if "<interactive>" `isPrefixOf` e'
     then file ++ drop 13 e'
     else e'
  where e' = errMsg e

interpretDoc :: Monad m => String -> String -> Interpreter (Doc m Block)
interpretDoc doc format = do
  loadModules ["Grammata/Format/" ++ format ++ ".hs", "Grammata/TH.hs"]
  set [languageExtensions := [TemplateHaskell]]
  setImportsQ [("Prelude", Nothing), ("Grammata.Format." ++ format, Nothing), ("Grammata.TH", Nothing), ("Data.String", Nothing), ("Language.Haskell.TH", Nothing)]
  let cmd = "heading"
  res <- parseDoc doc
  liftIO $ print res
  return (return $ Block $ T.pack "hi")

lookupCommand :: String -> Interpreter [TypeSpec]
lookupCommand cmd = do
  interpret ("$(toTypeSpec " ++ show cmd ++ ")") (as :: [TypeSpec])

type Parser = ParsecT [Char] () Interpreter
type CommandSpec = (String, [String])

pCommand :: Parser CommandSpec
pCommand = try $ do
  char '\\'
  cmd <- many1 alphaNum
  typespec <- lift $ lookupCommand cmd
  return (cmd, [show typespec])

pTrash :: Parser ()
pTrash = skipMany (noneOf "\\")

parseDoc :: String -> Interpreter (Either ParseError [CommandSpec])
parseDoc = runParserT (many $ try $ pTrash >> pCommand) () "input"


