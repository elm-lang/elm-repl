{-# LANGUAGE DeriveDataTypeable #-}
module Flags where

import System.Console.CmdArgs
import Data.Version (showVersion)
import qualified Paths_elm_repl as This

version = showVersion This.version

data Flags = Flags
    { compiler :: FilePath
    }
    deriving (Data,Typeable,Show,Eq)
             
flags = Flags
  { compiler = "elm" &= typFile
                     &= help "Provide a path to a specific Elm compiler."
  } &= help "Read-eval-print-loop (REPL) for digging deep into Elm projects.\n\
            \More info at <https://github.com/elm-lang/elm-repl#elm-repl>"
    &= helpArg [explicit, name "help", name "h"]
    &= versionArg [explicit, name "version", name "v", summary version]
    &= summary ("Elm REPL " ++ version ++ ", (c) Evan Czaplicki 2011-2013")
