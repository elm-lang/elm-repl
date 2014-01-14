module Main where

import Control.Monad
import Control.Monad.Trans
import System.Console.Haskeline
import System.Directory
import System.Exit
import System.FilePath ((</>))

import qualified System.Console.CmdArgs as CmdArgs

import Monad

import qualified Command     as Cmd
import qualified Completion
import qualified Evaluator   as Eval
import qualified Environment as Env
import qualified Flags

main :: IO ()
main = do
  flags <- CmdArgs.cmdArgs Flags.flags
  buildExisted <- doesDirectoryExist "build"
  cacheExisted <- doesDirectoryExist "cache"
  settings     <- mkSettings
  putStrLn welcomeMessage
  let mt = Env.empty (Flags.compiler flags)
  exitCode <- runReplM flags mt . runInputT settings . withInterrupt $ repl
  when (not buildExisted) (removeDirectoryRecursive "build")
  when (not cacheExisted) (removeDirectoryRecursive "cache")
  exitWith exitCode

repl :: InputT ReplM ExitCode
repl = do
  str' <- handleInterrupt (return . Just $ "") getInput
  case str' of
    Just (':':command) -> lift (Cmd.runCommand command) >> repl
    Just input         -> lift (Eval.evalPrint input)   >> repl
    Nothing            -> return ExitSuccess

getInput :: (MonadException m) => InputT m (Maybe String)
getInput = go "> " ""
    where
      go str old = do
        input <- getInputLine str
        case input of
          Nothing  -> return Nothing
          Just new -> continueWith (old ++ new)

      continueWith str
        | null str || last str /= '\\' = return $ Just str
        | otherwise = go "| " (init str ++ "\n")

welcomeMessage :: String
welcomeMessage =
    "Elm REPL " ++ Flags.version ++
    " <https://github.com/evancz/elm-repl#elm-repl>\n\
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
