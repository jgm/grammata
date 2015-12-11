{-# LANGUAGE OverloadedStrings #-}
module Grammata.Parse (interpretDoc) where

-- import Debug.Trace
import Control.Monad
import qualified Control.Monad.Catch as Catch
import Language.Haskell.Interpreter -- hint
import Data.String
import Data.Monoid
import Grammata.Types
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Text (Text)
import Data.List (isPrefixOf, isInfixOf)
import Data.List.Split (splitOn)
import Text.Parsec
import System.IO (stderr)
import Data.Char (isLower)

interpretDoc :: Int -> String -> String -> Interpreter (Doc IO Block)
interpretDoc verbosity doc format = do
  let hdrlines = takeWhile ("%%" `isPrefixOf`) $ lines doc
  let addFormat s = case reverse s of
                         '*':xs -> reverse xs ++ format
                         _ -> s
  let modules = map (addFormat . drop 2) hdrlines
  when (null modules) $
    Catch.throwM $ UnknownError "You have not specified any modules to load"
  loadModules modules
  set [languageExtensions := [TemplateHaskell, OverloadedStrings]]
  setImports ("Prelude" : "Data.String" : "Data.Monoid" :
               "Language.Haskell.TH" : "Grammata.Types" :
               "Control.Monad.RWS" : modules)
  res <- parseDoc doc
  case res of
       Left e  -> error (show e)
       Right r -> do
          when (verbosity >= 2) $ liftIO $ T.hPutStrLn stderr r
          interpret (T.unpack r) (as :: Doc IO Block)

lookupCommand :: String -> Interpreter (Maybe (String, [String]))
lookupCommand cmd = do
  xs <- typeSpecOf cmd
  return $
    if null xs
       then Nothing
       else Just (last xs, init xs) -- result, args

typeSpecOf :: String -> Interpreter [String]
typeSpecOf cmd = do
  tc <- typeChecks cmd
  if tc
     then do
       ty <- map (\c -> if c == '\n' then ' ' else c) <$> typeOf cmd
       return $ splitOn " -> " $
                 if " => " `isInfixOf` ty
                    then drop 3 $ dropWhile (/='=') ty
                    else ty
     else return []

type Parser = ParsecT [Char] () Interpreter

pControlSeq :: Parser String
pControlSeq = try $ do
  char '\\'
  cmd <- many1 alphaNum
  spaces
  return cmd

pInlineCommand :: Parser Text
pInlineCommand = try $ do
  cmd <- pControlSeq
  typespec <- lift $ lookupCommand cmd
  case typespec of
       Nothing               -> fail $ "Undefined command " ++ cmd
       Just (x, args) | isInline (lastWord  x) ->
          ((T.pack cmd <> " ") <>) <$> processArgs args
       Just _                -> fail $ cmd ++ " does not return Inline"

pBlockCommand :: Parser Text
pBlockCommand = try $ do
  cmd <- pControlSeq
  typespec <- lift $ lookupCommand cmd
  case typespec of
       Nothing              -> fail $ "Undefined command " ++ cmd
       Just (x, args) | isBlock (lastWord x) ->
          ((T.pack cmd <> " ") <>) <$> processArgs args
       Just _               -> fail $ cmd ++ " does not return Block"

isInline :: String -> Bool
isInline "Inline" = True
isInline "Grammata.Types.Inline" = True
isInline (c:_) = isLower c -- type variable
isInline _ = False

isBlock :: String -> Bool
isBlock "Block" = True
isBlock "Grammata.Types.Block" = True
isBlock (c:_) = isLower c -- type variable
isBlock _ = False

lastWord :: String -> String
lastWord [] = ""
lastWord xs = last . words $ xs

isTypeVar :: String -> Bool
isTypeVar (c:_) = isLower c
isTypeVar _ = False

processArgs :: [String] -> Parser Text
processArgs = fmap mconcat . mapM processArg

processArg :: String -> Parser Text
processArg x | "Doc" `isPrefixOf` x =
  case reverse (words x) of
       "Grammata.Types.Inline":_ -> pInlineArg
       "Inline":_ -> pInlineArg
       "Grammata.Types.Block":_  -> pBlockArg
       "Block":_  -> pBlockArg
       s:_ | isTypeVar s -> pInlineArg <|> pBlockArg
       _ -> fail $ "Unimplemented type " ++ x
processArg x = try $ do
  spaces
  char '{'
  cs <- many (noneOf "\\}" <|> try (char '\\' >> anyChar))
  char '}'
  res <- lift $ checkedRead cs x
  case res of
      Left e   -> fail e
      Right r  -> return $ T.pack r

pBraced :: Parser Text -> Parser Text
pBraced p = try $ do
  spaces
  char '{'
  inconcat <$> manyTill p (char '}')

inconcat :: [Text] -> Text
inconcat ts = "(" <> T.intercalate "\n<>\n" ts <> ")"

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
pInlineArg = pBraced (try $ skipMany pComment >> (pText <|> pInlineCommand)
                             <* skipMany pComment)

pBlockArg :: Parser Text
pBlockArg = pBraced (try $ skipMany (pComment <|> pSkip) >> pBlockCommand
                             <* skipMany (pComment <|> pSkip))

parseDoc :: String -> Interpreter (Either ParseError Text)
parseDoc = runParserT
  (T.intercalate "\n<>\n" <$>
    many (try $ skipMany (pComment <|> pSkip) >> pBlockCommand)
      <* spaces <* eof) () "input"

-- assumes ty is a string rep of a type with matched Show and Read
checkedRead :: String -> String -> Interpreter (Either String String)
checkedRead val ty = do
  res <- Catch.try $ interpret ("fmap show (toArg " ++
                                 show val ++ " :: Either String " ++ ty ++ ")")
              (as :: Either String String)
  case (res :: Either InterpreterError (Either String String)) of
     Right (Right x) -> return $ Right x
     Right (Left e)  -> return $ Left $
                          "Could not parse " ++ show val ++ " as " ++ ty ++
                          "\n" ++ e
     Left (WontCompile (e:_)) |
       ("No instance for (ToArg " ++ ty ++ ")") `isInfixOf` errMsg e
                      -> return $ Left $
                          "Could not parse " ++ show val ++ " as " ++ ty ++
                          "\nNo ToArg instance is defined for " ++ ty
     _                -> return $ Left $
                          "Could not parse " ++ show val ++ " as " ++ ty
