{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
import Control.Monad
import Language.Haskell.TH.Quote -- gives us QuasiQuoter
import Language.Haskell.TH       -- gives us stuff to return 'Q Exp's.
                                 -- ie. stringL, litE

import Grammata.Format.Html

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibsQ :: Q Exp
fibsQ = [| fibs |]

fibQ :: Int -> Q Exp
fibQ n = [| fibs !! n |]

mylit :: Q Exp
mylit = appE (varE (mkName "reverse")) (litE (StringL "hello"))

fibsQ' :: Q Exp
fibsQ' = varE (mkName "fibs")

emphQ :: Q Exp
emphQ = varE (mkName "emph")

-- display with $(stringE =<< foo "emph")
-- *Main> $(stringE =<< foo "emph")
-- "ClassOpI Grammata.Types.emph (ForallT [KindedTV f_1627440712 (AppT (AppT ArrowT StarT) StarT)] [AppT (ConT Grammata.Types.ToEmph) (VarT f_1627440712)] (AppT (AppT ArrowT (AppT (ConT Grammata.Types.Doc) (AppT (VarT f_1627440712) (ConT Grammata.Types.Inline)))) (AppT (ConT Grammata.Types.Doc) (AppT (VarT f_1627440712) (ConT Grammata.Types.Inline))))) Grammata.Types.ToEmph (Fixity 9 InfixL)"

-- in simple branch:
-- *Main Grammata.Format.Html> $(stringE =<< foo "emph")
-- "VarI Grammata.Format.Html.emph (AppT (AppT ArrowT (AppT (ConT Grammata.Types.Doc) (ConT Grammata.Types.Inline))) (AppT (ConT Grammata.Types.Doc) (ConT Grammata.Types.Inline))) Nothing (Fixity 9 InfixL)"

foo :: String -> Q String
foo x = do mbname <- lookupValueName x
           case mbname of
                 Just name -> reify name >>= return . show
                 _    -> return "??"

-- PLAN
-- evaluate separately using TH splices
-- *Main> let hi = "hi"
-- *Main> $(varE (mkName "emph")) $([|hi :: Doc (Html Grammata.Inline)|])
-- <Html "<em>hi</em>", DocState {vars = fromList []}>
-- Now, to do this we just need to figure out what the types of the
-- arguments are (e.g. Inline, String, Block) and fix the format type
-- (e.g. Html, TeX).
-- or could we keep it polymorphic?
-- *Main> $(varE (mkName "emph")) $([|"hi"|])
--
-- <interactive>:28:1:
--     No instance for (Show (f0 Grammata.Inline))
--
-- *Main Data.Monoid> $(varE (mkName "emph")) $([|"hi" <> emph "foo"|]) :: Doc (Html Grammata.Inline)
-- <Html "<em>hi<em>foo</em></em>", DocState {vars = fromList []}>
-- looks like we could keep it polymorphic til the end
--
-- so the procedure is:
-- parseInline :=
-- parse a bunch of text not including {, }, \\, OR
-- parse a command name
--   figure out its argument structure - fail if it doesn't yield Inline
--     for each argument, take material in braces, and:
--       if inline, mconcat <$> many parseInline
--       if block , mconcat <$> many parseBlock
--       if string, parseString
--   now use th splices to apply the associated function to the args

-- similarly parseBlock
--
-- SUPER-EASY way to get the type, now that we're monomorphic!
--
-- *Main Data.Typeable> typeOf $(varE (mkName "emph"))
-- RWST DocState () DocState Identity Inline -> RWST DocState () DocState Identity Inline
-- import Data.List.Split
--  splitOn " -> " $ show (typeOf $(varE (mkName "emph")))
--  ["RWST DocState () DocState Identity Inline","RWST DocState () DocState Identity Inline"]

