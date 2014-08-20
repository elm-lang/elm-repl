{-# LANGUAGE OverloadedStrings #-}
module Environment where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Monoid ((<>))
import Data.Trie (Trie) -- | TODO: Switch to a Char-based trie.
import qualified Data.Trie as Trie

import Action (Term)
import qualified Action as A

data Repl = Repl
    { compilerPath  :: FilePath
    , interpreterPath :: FilePath
    , flags :: [String]
    , imports :: Trie String
    , adts :: Trie String
    , defs :: Trie String
    } deriving Show

empty :: FilePath -> FilePath -> Repl
empty cmpPath interpPath =
    Repl cmpPath interpPath [] Trie.empty Trie.empty (Trie.singleton firstVar (BS.unpack firstVar <> " = ()"))

firstVar :: ByteString
firstVar = "tsol"

lastVar :: ByteString
lastVar = "deltron3030"

toElm :: Repl -> String
toElm env = unlines $ "module Repl where" : decls
    where decls = concatMap Trie.elems [ imports env, adts env, defs env ]

insert :: Term -> Repl -> Repl
insert (src, def) env = case def of
  Nothing -> display src env
  Just (A.Import mport) -> noDisplay $ env { imports = Trie.insert (BS.pack mport) src (imports env) }
  Just (A.DataDef def)  -> noDisplay $ env { adts    = Trie.insert (BS.pack def)   src (adts env) }
  Just (A.VarDef var)   -> define (BS.pack var) src . display var $ env

define :: ByteString -> String -> Repl -> Repl
define name body env = env { defs = Trie.insert name body (defs env) }

display :: String -> Repl -> Repl
display = define lastVar . format
  where format body = (BS.unpack lastVar) ++ " =" ++ concatMap ("\n  "++) (lines body)

noDisplay :: Repl -> Repl
noDisplay env = env { defs = Trie.delete lastVar (defs env) }
