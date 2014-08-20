module Main where

import Control.Monad (unless, when)
import qualified System.Console.CmdArgs as CmdArgs
import System.Console.Haskeline (Settings(Settings, autoAddHistory, complete,
                                          historyFile))
import qualified System.Directory as Dir
import qualified System.Exit as Exit
import System.FilePath ((</>))

import qualified Completion
import qualified Flags
import Monad (ReplM)
import qualified Repl

main :: IO ()
main = do
  flags <- CmdArgs.cmdArgs Flags.flags
  buildExisted <- Dir.doesDirectoryExist "build"
  cacheExisted <- Dir.doesDirectoryExist "cache"
  settings     <- mkSettings
  putStrLn welcomeMessage
  exitCode <- ifJsInterpExists flags (Repl.run flags settings)
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
  dir <- (</> "repl") `fmap` Dir.getAppUserDataDirectory "elm"
  Dir.createDirectoryIfMissing True dir
  return dir

mkSettings :: IO (Settings ReplM)
mkSettings = do
  historyFile <- (</> "history") `fmap` elmdir
  return $ Settings { historyFile    = Just historyFile
                    , autoAddHistory = True
                    , complete       = Completion.complete
                    }

ifJsInterpExists :: Flags.Flags -> IO Exit.ExitCode -> IO Exit.ExitCode
ifJsInterpExists flags doSomeStuff =
  do maybePath <- Dir.findExecutable jsInterp
     case maybePath of
       Just _  -> doSomeStuff
       Nothing ->
           do putStrLn interpNotInstalledMessage
              return (Exit.ExitFailure 1)
  where
    jsInterp = Flags.js_cmd flags

    interpNotInstalledMessage =
        "\n\
        \The REPL relies on node.js to execute JavaScript code outside the browser.\n\
        \    It appears that you do not have node.js installed though!\n\
        \    Install node.js from <http://nodejs.org/> to use elm-repl."
        
removeDirectoryRecursiveIfExists :: FilePath -> IO ()
removeDirectoryRecursiveIfExists path =
    do exists <- Dir.doesDirectoryExist path
       when exists (Dir.removeDirectoryRecursive path)
