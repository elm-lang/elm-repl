{-# LANGUAGE DeriveDataTypeable #-}
module Flags where

import Data.Version (showVersion)
import qualified Paths_elm_repl as This
import System.Console.CmdArgs
    ( Data, Typeable, (&=), explicit, help, helpArg
    , name, summary, typFile, versionArg
    )

import qualified Elm.Compiler as Compiler
import qualified Elm.Package as Pkg


version :: String
version =
  Pkg.versionToString Compiler.version


data Flags = Flags
    { compiler :: FilePath
    , interpreter :: Maybe FilePath
    }
    deriving (Data,Typeable,Show,Eq)


flags :: Flags
flags = Flags
  { compiler = "elm-make"
      &= typFile
      &= help "Provide a path to a specific version of elm-make."

  , interpreter = Nothing
      &= typFile
      &= help "Provide a path to a specific JavaScript interpreter (e.g. node, nodejs, ...)."
  }
  &= help helpMessage

  &= helpArg
      [ explicit
      , name "help"
      , name "h"
      ]

  &= versionArg
      [ explicit
      , name "version"
      , name "v"
      , summary version
      ]

  &= summary ("elm repl " ++ version)


helpMessage :: String
helpMessage =
    "Read-eval-print-loop (REPL) for digging deep into Elm projects.\n\
    \More info at <https://github.com/elm-lang/elm-repl#elm-repl>"