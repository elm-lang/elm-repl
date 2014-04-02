module Main where

import Control.Monad
import System.Console.Haskeline hiding (handle)
import System.Directory
import System.Exit
import System.FilePath ((</>))

import qualified System.Console.CmdArgs as CmdArgs

import Monad

import qualified Repl
import qualified Completion
import qualified Flags

main :: IO ()
main = do
  flags <- CmdArgs.cmdArgs Flags.flags
  buildExisted <- doesDirectoryExist "build"
  cacheExisted <- doesDirectoryExist "cache"
  settings     <- mkSettings
  putStrLn welcomeMessage
  exitCode <- Repl.run flags settings
  unless buildExisted (removeDirectoryRecursive "build")
  unless cacheExisted (removeDirectoryRecursive "cache")
  exitWith exitCode

welcomeMessage :: String
welcomeMessage =
    "Elm REPL " ++ Flags.version ++
    " <https://github.com/elm-lang/elm-repl#elm-repl>\n\
    \Type :help for help, :exit to exit"

elmdir :: IO FilePath
elmdir = do
  dir <- (</> "repl") `fmap` getAppUserDataDirectory "elm"
  createDirectoryIfMissing True dir
  return dir

mkSettings :: IO (Settings ReplM)
mkSettings = do
  historyFile <- (</> "history") `fmap` elmdir
  return $ Settings { historyFile    = Just historyFile
                    , autoAddHistory = True
                    , complete       = Completion.complete
                    }
