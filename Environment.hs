{-# LANGUAGE OverloadedStrings #-}
module Environment where

import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.List
import qualified Data.Map as Map

data Repl = Repl
    { imports :: Map.Map String String
    , adts :: Map.Map String String
    , defs :: Map.Map String String
    } deriving Show

empty :: Repl
empty = Repl Map.empty Map.empty Map.empty

expr :: BS.ByteString
expr = BS.pack internalExpr

internalExpr :: String
internalExpr = "deltron3030"

toElm :: Repl -> String
toElm env = unlines $ "module Repl where" : decls
    where decls = concatMap Map.elems [ imports env, adts env, defs env ]

insert :: String -> Repl -> Repl
insert str env
    | isPrefixOf "import " str = 
        let name = getFirstCap (words str)
            getFirstCap (token@(c:_):rest) =
                if isUpper c then token else getFirstCap rest
            getFirstCap _ = str
        in  env { imports = Map.insert name str (imports env) }

    | isPrefixOf "data " str =
        let name = takeWhile (/=' ') (drop 5 str)
        in  env { adts = Map.insert name str (adts env) }
            
    | otherwise =
        case break (=='=') str of
          (_,"") -> addExpr str env
          (beforeEquals, _:c:_)
              | isSymbol c || hasLet beforeEquals -> addExpr str env
              | otherwise -> let name = declName beforeEquals
                             in  add name str (addExpr name env)

        where
          add name body env = env { defs = Map.insert name body (defs env) }
          addExpr body = add internalExpr (format body)

          format body = internalExpr ++ " =" ++ concatMap ("\n  "++) (lines body)

          declName pattern =
              case takeWhile isSymbol . dropWhile (not . isSymbol) $ pattern of
                "" -> takeWhile (/=' ') pattern
                op -> op

          hasLet = any (=="let") . map token . words
              where
                isVarChar c = isAlpha c || isDigit c || elem c "_'"
                token = takeWhile isVarChar . dropWhile (not . isAlpha)