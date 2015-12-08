import Language.Haskell.Interpreter -- hint
import Data.String
import Grammata.Types
import qualified Data.Text.IO as T
import Data.List (isPrefixOf)
import System.Environment
import Data.List.Split
import Text.Parsec

main :: IO ()
main = do
  args <- getArgs
  progname <- getProgName
  let (format,file) = case args of
                       [x,y] -> (x,y)
                       _ -> error $ "Usage:  " ++ progname ++ " [TeX|Html] file"

  docu <- readFile file

  r <- runInterpreter (interpretDoc docu format)
  case r of
       Left (WontCompile es) -> mapM_ (putStrLn . showCompileError file) es
       Left err -> putStrLn (show err)
       Right x -> liftIO . T.putStrLn . render $ x

showCompileError :: [Char] -> GhcError -> [Char]
showCompileError file e =
  if "<interactive>" `isPrefixOf` e'
     then file ++ drop 13 e'
     else e'
  where e' = errMsg e

interpretDoc :: String -> String -> Interpreter (Doc Block)
interpretDoc docu format = do
  loadModules ["Grammata.hs", "Grammata/Format/" ++ format ++ ".hs"]
  set [languageExtensions := [OverloadedStrings, TemplateHaskell, QuasiQuotes]]
  setImportsQ [("Prelude", Nothing), ("Data.Monoid", Nothing), ("Control.Monad.RWS", Nothing), ("Control.Monad.Identity", Nothing), ("Grammata", Nothing), ("Grammata.Format." ++ format, Nothing), ("Data.String", Nothing), ("Language.Haskell.TH", Nothing), ("Data.Typeable", Nothing)]
  let cmd = "heading"
  return . doc . Block . fromString . show =<< parseDoc docu

lookupCommand :: String -> Interpreter [TypeSpec]
lookupCommand cmd =
  map shortenType . splitType <$> interpret ("show $ typeOf $(varE (mkName " ++ show cmd ++ "))") (as :: String)

type TypeSpec = String

shortenType :: String -> String
shortenType s
  | "RWST DocState () DocState Identity" `isPrefixOf` s =
    "Doc " ++ drop 35 s
  | otherwise = s

splitType :: String -> [TypeSpec]
splitType = splitOn " -> "

type Parser = ParsecT [Char] () Interpreter
type CommandSpec = (String, [String])

pCommand :: Parser CommandSpec
pCommand = try $ do
  char '\\'
  cmd <- many1 alphaNum
  typespec <- lift $ lookupCommand cmd
  return (cmd, typespec)

pTrash :: Parser ()
pTrash = skipMany (noneOf "\\")

parseDoc :: String -> Interpreter (Either ParseError [CommandSpec])
parseDoc = runParserT (many $ try $ pTrash >> pCommand) () "input"

