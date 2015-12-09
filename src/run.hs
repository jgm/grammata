{-# LANGUAGE OverloadedStrings #-}
import Debug.Trace
import Language.Haskell.Interpreter -- hint
import Data.Monoid
import Data.String
import Control.Monad
import Grammata.Types
import Grammata.TH
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Text (Text)
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

interpretDoc :: String -> String -> Interpreter (Doc IO Block)
interpretDoc doc format = do
  loadModules ["Grammata/Format/" ++ format ++ ".hs", "Grammata/TH.hs"]
  set [languageExtensions := [TemplateHaskell]]
  setImportsQ [("Prelude", Nothing), ("Grammata.Format." ++ format, Nothing), ("Grammata.TH", Nothing), ("Data.String", Nothing), ("Language.Haskell.TH", Nothing)]
  res <- parseDoc doc
  case res of
       Left e  -> error (show e)
       Right r -> return $ return $ Block r

lookupCommand :: Text -> Interpreter (Maybe (String, [String]))
lookupCommand cmd = do
  xs <- interpret ("$(toTypeSpec " ++ show cmd ++ ")") (as :: [String])
  return $
    if null xs
       then Nothing
       else Just (last xs, init xs) -- result, args

type Parser = ParsecT [Char] () Interpreter

pControlSeq :: Parser Text
pControlSeq = try $ do
  char '\\'
  cmd <- many1 alphaNum
  spaces
  return $ fromString cmd

pInlineCommand :: Parser Text
pInlineCommand = try $ do
  cmd <- pControlSeq
  typespec <- lift $ lookupCommand cmd
  case typespec of
       Nothing               -> fail $ "Undefined command " ++ T.unpack cmd
       Just ("Inline", args) -> ((cmd <> " ") <>) <$> processArgs args
       Just _                -> fail $ T.unpack cmd ++ " does not return Inline"

pBlockCommand :: Parser Text
pBlockCommand = try $ do
  cmd <- pControlSeq
  typespec <- lift $ lookupCommand cmd
  case typespec of
       Nothing              -> fail $ "Undefined command " ++ T.unpack cmd
       Just ("Block", args) -> ((cmd <> " ") <>) <$> processArgs args
       Just _               -> fail $ T.unpack cmd ++ " does not return Block"

processArgs :: [String] -> Parser Text
processArgs = fmap mconcat . mapM processArg

processArg :: String -> Parser Text
processArg "Inline" = pInlineArg
processArg "Block" = pBlockArg
processArg x = fail $ "Argument type "  ++ x ++ " unimplemented"

pBraced :: Parser Text -> Parser Text
pBraced p = do
  char '{'
  (inParens . T.intercalate " <> ") <$> manyTill p (char '}')

inParens :: Text -> Text
inParens t = "(" <> t <> ")"

pComment :: Parser ()
pComment = do
  char '%'
  skipMany (noneOf "\n")
  optional (char '\n')

pSkip :: Parser ()
pSkip = space >> spaces

pText :: Parser Text
pText = (fromString . show) <$> many1 (noneOf "\\{}%")

pInlineArg :: Parser Text
pInlineArg = pBraced (try $ skipMany pComment >> (pText <|> pInlineCommand))

pBlockArg :: Parser Text
pBlockArg = pBraced (try $ skipMany (pComment <|> pSkip) >> pBlockCommand)

parseDoc :: String -> Interpreter (Either ParseError Text)
parseDoc = runParserT
  (T.intercalate " <> " <$>
    many (try $ skipMany (pComment <|> pSkip) >> pBlockCommand)
      <* spaces <* eof) () "input"

