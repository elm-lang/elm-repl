{-# LANGUAGE DeriveDataTypeable #-}
module Flags where

import Data.Version (showVersion)
import qualified Paths_elm_repl as This
import System.Console.CmdArgs (Data, Typeable, (&=), explicit, help, helpArg,
                               name, summary, typFile, versionArg)

version = showVersion This.version

data Flags = Flags
    { compiler :: FilePath
    , interpreter :: FilePath
    }
    deriving (Data,Typeable,Show,Eq)

flags :: Flags             
flags = Flags
    { compiler = "elm"
        &= typFile
        &= help "Provide a path to a specific Elm compiler."
    , interpreter = "node" 
        &= typFile
        &= help "Provide a path to a specific JavaScript interpreter (e.g. node, nodejs, ...)."
    }
        &= help helpMessage

        &= helpArg [explicit, name "help", name "h"]

        &= versionArg [explicit, name "version", name "v", summary version]

        &= summary ("Elm REPL " ++ version ++ ", (c) Evan Czaplicki 2011-2013")

helpMessage :: String
helpMessage =
    "Read-eval-print-loop (REPL) for digging deep into Elm projects.\n\
    \More info at <https://github.com/elm-lang/elm-repl#elm-repl>"