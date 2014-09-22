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
import qualified Loop


main :: IO ()
main =
 do flags <- CmdArgs.cmdArgs Flags.flags
    buildExisted <- Dir.doesDirectoryExist "build"
    cacheExisted <- Dir.doesDirectoryExist "cache"
    settings     <- mkSettings
    putStrLn welcomeMessage
    exitCode <- ifJsInterpExists flags (Loop.loop flags settings)
    unless buildExisted (removeDirectoryRecursiveIfExists "build")
    unless cacheExisted (removeDirectoryRecursiveIfExists "cache")
    Exit.exitWith exitCode


welcomeMessage :: String
welcomeMessage =
    "Elm REPL " ++ Flags.version ++ " <https://github.com/elm-lang/elm-repl#elm-repl>\n\
    \Type :help for help, :exit to exit"


getDataDir :: IO FilePath
getDataDir =
 do root <- Dir.getAppUserDataDirectory "elm"
    let dir = root </> "repl"
    Dir.createDirectoryIfMissing True dir
    return dir


mkSettings :: IO (Settings ReplM)
mkSettings =
 do dataDir <- getDataDir
    return $ Settings
        { historyFile    = Just (dataDir </> "history")
        , autoAddHistory = True
        , complete       = Completion.complete
        }


ifJsInterpExists :: Flags.Flags -> IO Exit.ExitCode -> IO Exit.ExitCode
ifJsInterpExists flags doSomeStuff =
 do maybePath <- Dir.findExecutable jsInterp
    case maybePath of
      Just _  -> doSomeStuff
      Nothing ->
        do  putStrLn interpNotInstalledMessage
            return (Exit.ExitFailure 1)
  where
    jsInterp = Flags.interpreter flags

    interpNotInstalledMessage =
        "\n\
        \The REPL relies on node.js to execute JavaScript code outside the browser.\n\
        \    It appears that you do not have node.js installed though!\n\
        \    You can install node.js from <http://nodejs.org/>. If it is already\n\
        \    installed but has a different name, use the --interpreter flag."

        
removeDirectoryRecursiveIfExists :: FilePath -> IO ()
removeDirectoryRecursiveIfExists path =
 do exists <- Dir.doesDirectoryExist path
    when exists (Dir.removeDirectoryRecursive path)
