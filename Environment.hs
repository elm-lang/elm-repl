{-# LANGUAGE OverloadedStrings #-}
module Environment where

import Data.ByteString (ByteString)
import Data.Monoid     ((<>))
import qualified Data.ByteString.Char8 as BS
import qualified Data.Char             as Char
import qualified Data.List             as List
import Data.Trie       (Trie)
import qualified Data.Trie             as Trie

data Repl = Repl
    { compilerPath :: FilePath
    , flags :: [String]
    , imports :: Trie String
    , adts :: Trie String
    , defs :: Trie String
    } deriving Show

empty :: FilePath -> Repl
empty compilerPath =
    Repl compilerPath [] Trie.empty Trie.empty (Trie.singleton firstVar (BS.unpack firstVar <> " = ()"))

firstVar :: ByteString
firstVar = "tsol"

lastVar :: ByteString
lastVar = "deltron3030"

toElm :: Repl -> String
toElm env = unlines $ "module Repl where" : decls
    where decls = concatMap Trie.elems [ imports env, adts env, defs env ]

insert :: String -> Repl -> Repl
insert str env
    | List.isPrefixOf "import " str = 
      let name = BS.pack . getFirstCap . words $ str
          getFirstCap (token@(c:_):rest) = if Char.isUpper c
                                           then token
                                           else getFirstCap rest
          getFirstCap _ = str
      in  noDisplay $ env { imports = Trie.insert name str (imports env) }

    | List.isPrefixOf "data " str =
        let name = BS.pack . takeWhile (/=' ') . drop 5 $ str
        in  noDisplay $ env { adts = Trie.insert name str (adts env) }
            
    | otherwise =
        case break (=='=') str of
          (_,"") -> display str env
          (beforeEquals, _:c:_)
              | Char.isSymbol c || hasLet beforeEquals || hasBrace beforeEquals -> display str env
              | otherwise -> let name = declName $ beforeEquals
                             in  define (BS.pack name) str (display name env)
          _ -> error "Environment.hs: Case error. Submit bug report."
        where
          declName pattern =
              case takeWhile Char.isSymbol . dropWhile (not . Char.isSymbol) $ pattern of
                "" -> takeWhile (/=' ') pattern
                op -> op

          hasLet = elem "let" . map token . words
            where
              isVarChar c = Char.isAlpha c || Char.isDigit c || elem c "_'"
              token = takeWhile isVarChar . dropWhile (not . Char.isAlpha)

          hasBrace = elem '{'

define :: ByteString -> String -> Repl -> Repl
define name body env = env { defs = Trie.insert name body (defs env) }

display :: String -> Repl -> Repl
display = define lastVar . format
  where format body = (BS.unpack lastVar) ++ " =" ++ concatMap ("\n  "++) (lines body)

noDisplay :: Repl -> Repl
noDisplay env = env { defs = Trie.delete lastVar (defs env) }
