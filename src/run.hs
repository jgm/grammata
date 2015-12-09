import Debug.Trace
import Language.Haskell.Interpreter -- hint
import Data.String
import Control.Monad
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
  res <- parseDoc doc
  case res of
       Left e  -> error (show e)
       Right r -> return r

lookupCommand :: String -> Interpreter [String]
lookupCommand cmd = do
  interpret ("$(toTypeSpec " ++ show cmd ++ ")") (as :: [String])

type Parser = ParsecT [Char] () Interpreter

pControlSeq :: Parser String
pControlSeq = try $ do
  char '\\'
  cmd <- many1 alphaNum
  spaces
  return cmd

pInlineCommand :: Monad m => Parser (Doc m Inline)
pInlineCommand = try $ do
  cmd <- pControlSeq
  typespec <- lift $ lookupCommand cmd
  return $ fromString $ show (cmd, typespec)

pBlockCommand :: Monad m => Parser (Doc m Block)
pBlockCommand = try $ do
  cmd <- pControlSeq
  typespec <- lift $ lookupCommand cmd
  return $ return . Block . T.pack $ show (cmd, typespec)

pBraced :: (Monad m, Monoid a) => Parser (Doc m a) -> Parser (Doc m a)
pBraced p = do
  char '{'
  mconcat <$> manyTill p (char '}')

pComment :: (Monad m, Monoid a) => Parser (Doc m a)
pComment = do
  char '%'
  skipMany (noneOf "\n")
  optional (char '\n')
  return mempty

pSkip :: Monad m => Parser (Doc m Block)
pSkip = space >> spaces >> return mempty

pText :: Monad m => Parser (Doc m Inline)
pText = fromString <$> many1 (noneOf "\\{}%")

pInlineArg :: Monad m => Parser (Doc m Inline)
pInlineArg = pBraced (pText <|> pInlineCommand <|> pComment)

pBlockArg :: Monad m => Parser (Doc m Block)
pBlockArg = pBraced (pBlockCommand <|> pComment <|> (spaces >> return mempty))

parseDoc :: Monad m => String -> Interpreter (Either ParseError (Doc m Block))
parseDoc = runParserT
  (mconcat <$> many (pBlockCommand <|> pComment <|> (anyChar >> return mempty)) <* eof) () "input"

