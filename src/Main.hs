module Main where

import Control.Monad
import System.Console.Haskeline hiding (handle)
import System.Directory
import qualified System.Exit as Exit
import System.FilePath ((</>))
import qualified System.Process as Process

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
  exitCode <- ifNodeIsInstalled (Repl.run flags settings)
  unless buildExisted (removeDirectoryRecursiveIfExists "build")
  unless cacheExisted (removeDirectoryRecursiveIfExists "cache")
  Exit.exitWith exitCode

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

ifNodeIsInstalled :: IO Exit.ExitCode -> IO Exit.ExitCode
ifNodeIsInstalled doSomeStuff =
  do (exitCode, _, _) <- Process.readProcessWithExitCode "node" ["-v"] ""
     case exitCode of
       Exit.ExitFailure code| code `elem` [127,9009] ->
           do putStrLn nodeNotInstalledMessage
              return (Exit.ExitFailure 1)

       _ -> doSomeStuff
  where
    nodeNotInstalledMessage =
        "The REPL relies on node.js to execute JavaScript code outside the browser.\n\
        \    It appears that you do not have node.js installed though!\n\
        \    Install node.js from <http://nodejs.org/> to use elm-repl."
        
removeDirectoryRecursiveIfExists :: FilePath -> IO ()
removeDirectoryRecursiveIfExists path =
    do exists <- doesDirectoryExist path
       when exists (removeDirectoryRecursive path)
