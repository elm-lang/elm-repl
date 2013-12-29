{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.Trans
import Data.Functor ((<$>))
import System.Console.Haskeline
import qualified Evaluator as Eval
import qualified Environment as Env
import qualified System.Console.CmdArgs as CmdArgs
import System.Directory
import System.Environment
import System.Exit
import System.FilePath ((</>))

import Text.Parsec hiding (getInput)
import qualified Data.Map as Map
import qualified Flags

data Command
    = AddFlag Env.Flag
    | RemoveFlag Env.FlagKey
    | ListFlags
    | ClearFlags
    | InfoFlags
    | Help
    | Exit
    | Reset
      deriving Show

welcomeMessage = "Elm REPL\n\
                 \Type :help for help, Ctrl-d to exit"

elmdir :: IO FilePath
elmdir = do
  dir <- (</> "repl") <$> getAppUserDataDirectory "elm"
  createDirectoryIfMissing True dir
  return dir

mkSettings :: (MonadIO m) => IO (Settings m)
mkSettings = do
  historyFile <- (</> "history") <$> elmdir
  return $ defaultSettings { historyFile = Just historyFile }

main :: IO ()
main = do
  flags <- CmdArgs.cmdArgs Flags.flags
  buildExisted <- doesDirectoryExist "build"
  cacheExisted <- doesDirectoryExist "cache"
  settings     <- mkSettings
  putStrLn welcomeMessage
  exitCode <- runInputT settings $ withInterrupt $ loop (Env.empty (Flags.compiler flags))
  when (not buildExisted) (removeDirectoryRecursive "build")
  when (not cacheExisted) (removeDirectoryRecursive "cache")
  exitWith exitCode

loop :: Env.Repl -> InputT IO ExitCode
loop environment = do
  str' <- handleInterrupt (return . Just $ "") getInput
  case str' of
    Just (':':command) -> runCommand environment command
    Just input         -> loop =<< liftIO (Eval.runRepl input environment)
    Nothing            -> return ExitSuccess

getInput :: InputT IO (Maybe String)
getInput = get "> " ""
    where
      get str old = do
        input <- getInputLine str
        case input of
          Nothing  -> return Nothing
          Just new -> continueWith (old ++ new)

      continueWith str
        | null str || last str /= '\\' = return $ Just str
        | otherwise = get "| " (init str ++ "\n")
                      
runCommand :: Env.Repl -> String -> InputT IO ExitCode
runCommand env command = 
  case parseCommand command of
    Left err -> do
      lift . putStrLn $ "Could not parse command '" ++ command ++ "':"
      lift . putStrLn $ show err
      loop env
    Right command -> do
      let (env', sideEffects) = 
            case command of             
              AddFlag flag -> 
                let n = (Env.nextKey env) in
                (env { Env.flags = Map.insert (Env.nextKey env) flag (Env.flags env) }, putStrLn . Env.formatFlag $ (n, flag))
              RemoveFlag n -> 
                let flag = Map.lookup n (Env.flags env) in
                case flag of
                  Nothing -> (env, putStrLn "No such flag.")
                  Just f -> (env {Env.flags = Map.delete n $ Env.flags env}, putStrLn . Env.formatFlag $ (n, f))
              ListFlags -> (env, mapM_ (putStrLn . Env.formatFlag) . Map.toList $ (Env.flags env))
              ClearFlags -> (env {Env.flags = Map.empty}, putStrLn "All flags cleared")
              InfoFlags -> (env, putStrLn flagsInfo)
              Exit -> (env, exitSuccess)
              Reset -> (Env.reset (Env.compilerPath env) (Env.rootDirectory env), putStrLn "Environment Reset")
              Help -> (env, putStrLn helpInfo)
      lift sideEffects
      loop env'

parseCommand :: String -> Either ParseError Command
parseCommand str = parse commands "" str

commands = choice (flags : basics)
    where
      basics = map basicCommand [ ("exit",Exit), ("reset",Reset), ("help",Help) ]

      basicCommand (name,command) =
          string name >> spaces >> eof >> return command

flags = do
  string "flags"
  many space
  choice [ do string "add" >> many1 space >> srcDir
         , do string "remove" >> many1 space
              n <- many1 digit
              return $ RemoveFlag (read n)
         , do string "list"
              return ListFlags
         , do string "clear"
              return ClearFlags
         , return InfoFlags
         ]

srcDir = do
  string "--src-dir="
  v <- manyTill anyChar (choice [ space >> return (), eof ])
  return $ AddFlag ("src-dir",v)
  
flagsInfo = "Usage: flags [operation]\n\
            \\n\
            \  operations:\n\
            \    add --src-dir=FILEPATH\tAdd a compiler flag\n\
            \    list\t\t\tList all flags that have been added\n\
            \    remove id\t\t\tRemove a flag by its id\n\
            \    clear\t\t\tClears all flags\n" 

helpInfo = "General usage directions: <https://github.com/evancz/elm-repl#elm-repl>\n\
           \Additional commands available from the prompt:\n\
           \\n\
           \  :help\t\t\tList available commands\n\
           \  :flags\t\tManipulate flags sent to elm compiler\n\
           \  :reset\t\tClears all previous imports\n\
           \  :exit\t\t\tExits elm-repl\n"
