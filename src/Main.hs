module Main where

import Control.Applicative ((<|>))
import Control.Monad (when)
import qualified System.Console.CmdArgs as CmdArgs
import System.Console.Haskeline
  ( Settings(Settings, autoAddHistory, complete, historyFile)
  )
import qualified System.Directory as Dir
import qualified System.Exit as Exit
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

import qualified Completion
import qualified Environment as Env
import qualified Flags
import qualified Loop
import qualified Elm.Compiler as Elm
import qualified Elm.Package as Pkg


main :: IO ()
main =
  do  flags <- CmdArgs.cmdArgs Flags.flags
      stuffExisted <- Dir.doesDirectoryExist "elm-stuff"
      pkgJsonExisted <- Dir.doesFileExist "elm-package.json"
      exitCode <- runRepl flags
      when (not stuffExisted) (removeDirectoryRecursiveIfExists "elm-stuff")
      when (not pkgJsonExisted) (removeFileIfExists "elm-package.json")
      Exit.exitWith exitCode


-- CLEANUP

removeDirectoryRecursiveIfExists :: FilePath -> IO ()
removeDirectoryRecursiveIfExists path =
  do  exists <- Dir.doesDirectoryExist path
      when exists (Dir.removeDirectoryRecursive path)


removeFileIfExists :: FilePath -> IO ()
removeFileIfExists path =
  do  exists <- Dir.doesFileExist path
      when exists (Dir.removeFile path)


-- RUN THE REPL

runRepl :: Flags.Flags -> IO Exit.ExitCode
runRepl flags =
  do  putStrLn welcomeMessage
      (name, maybeInterpreter) <- findExe (Flags.interpreter flags)
      case maybeInterpreter of
        Nothing ->
            do  hPutStrLn stderr (exeNotFound name)
                return (Exit.ExitFailure 1)

        Just interpreter ->
            do  settings <- initSettings
                let env = Env.empty (Flags.compiler flags) interpreter
                Loop.loop env settings


-- FIND JS INTERPRETER

findExe :: Maybe String -> IO (String, Maybe FilePath)
findExe maybeName =
  case maybeName of
    Just name ->
        (,) name <$> Dir.findExecutable name

    Nothing ->
        do  maybeInterpreter <-
              (<|>) <$> Dir.findExecutable "node"
                    <*> Dir.findExecutable "nodejs"

            return ("node' or 'nodejs", maybeInterpreter)


exeNotFound :: String -> String
exeNotFound stuff =
    "The REPL relies on node.js to execute JavaScript code outside the browser.\n"
    ++ "I could not find executable '" ++ stuff ++ "' on your computer though!\n\n"
    ++ "You can install node.js from <http://nodejs.org/>. If it is already installed\n"
    ++ "but has a different name, use the --interpreter flag."


-- WELCOME

welcomeMessage :: String
welcomeMessage =
  let
    starter =
      "---- elm-repl " ++ Pkg.versionToString Elm.version ++ " "
  in
    starter ++ replicate (80 - length starter) '-' ++ "\n"
    ++ " :help for help, :exit to exit, more at <https://github.com/elm-lang/elm-repl>\n"
    ++ "--------------------------------------------------------------------------------"


-- SETTINGS

initSettings :: IO (Settings Env.Task)
initSettings =
  do  dataDir <- getDataDir
      return $ Settings
        { historyFile = Just (dataDir </> "history")
        , autoAddHistory = True
        , complete = Completion.complete
        }


getDataDir :: IO FilePath
getDataDir =
  do  root <- Dir.getAppUserDataDirectory "elm"
      let dir = root </> Pkg.versionToString Elm.version </> "repl"
      Dir.createDirectoryIfMissing True dir
      return dir

