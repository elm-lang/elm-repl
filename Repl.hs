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
  exitCode <- runReplM flags mt $ runInputT settings . withInterrupt . loop $ mt
  when (not buildExisted) (removeDirectoryRecursive "build")
  when (not cacheExisted) (removeDirectoryRecursive "cache")
  exitWith exitCode

loop :: Env.Repl -> InputT ReplM ExitCode
loop env = do
  str' <- handleInterrupt (return . Just $ "") getInput
  case str' of
    Just (':':command) -> lift   (Cmd.runCommand command) >>= kont
    Just input         -> liftIO (Eval.runRepl input env)     >>= loop
    Nothing            -> return ExitSuccess
  where kont = maybe (loop env) (liftIO . exitWith)

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

mkSettings :: (MonadIO m) => IO (Settings m)
mkSettings = do
  historyFile <- (</> "history") `fmap` elmdir
  return $ defaultSettings { historyFile = Just historyFile }

