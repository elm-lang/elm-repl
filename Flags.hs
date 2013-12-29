{-# LANGUAGE DeriveDataTypeable #-}
module Flags where

import System.Console.CmdArgs
import Data.Version (showVersion)
import qualified Paths_elm_repl as This

data Flags = Flags
    { compiler :: FilePath
    }
    deriving (Data,Typeable,Show,Eq)
             
flags = Flags
  { compiler = "elm" &= typFile
              &= help "Provide a path to a specific Elm compiler."
  } &= help "Read-eval-print-loop (REPL) for digging deep into Elm projects.\n\
            \More info at <https://github.com/evancz/elm-repl#elm-repl>"
    &= helpArg [explicit, name "help", name "h"]
    &= versionArg [explicit, name "version", name "v", summary (showVersion This.version)]
    &= summary ("Elm REPL " ++ showVersion This.version ++ ", (c) Evan Czaplicki 2011-2013")
