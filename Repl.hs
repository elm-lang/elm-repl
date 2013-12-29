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

commands = flags <|> reset <|> exit <|> help

flags = do
  _ <- string "flags"
  _ <- many space
  p <- flagoperation
  return p

flagoperation = addflag <|> removeflag <|> listflags <|> clearflags <|> infoflags

infoflags = return InfoFlags

addflag = do
  _ <- string "add"
  _ <- many1 space
  property

removeflag = do
  _ <- string "remove"
  _ <- many1 space
  n <- many1 digit
  return (RemoveFlag . read $ n)
  
listflags = do
  _ <- string "list"
  return ListFlags
  
clearflags = do
  _ <- string "clear"
  return ClearFlags

endOfInput = space <|> eof'

eof' = eof >> return ' '

property = srcdir

srcdir = do
  _ <- string "--src-dir="
  v <- manyTill anyChar endOfInput
  return (AddFlag ("src-dir", v))
  
exit = basicCommand "exit" Exit
  
reset = basicCommand "reset" Reset
  
help = basicCommand "help" Help

basicCommand c const = do
  _ <- string c
  _ <- spaces
  _ <- eof
  return const                      
  
flagsInfo = "Usage: flags [operation]\n\n" ++
            "  operations:\n" ++
            "    add --property=value\tSets a flag with the specified property.\n" ++
            "    remove flag-id\t\tRemoves a flag by its id.\n" ++
            "    list\t\t\tLists all flags and their ids.\n" ++
            "    clear\t\t\tClears all flags.\n\n" ++
            "  properties:\n" ++
            "    src-dir\t\t\tAdds a source directory to be searched during evaluation." 
{-            
            "  examples:\n" ++
            "    Add \"some/dir\" to the path of searched directories\n" ++
            "    > :flags set src-dir=some/dir\n" ++
            "    0: src-dir=some/dir\n\n" ++
            "    Add \"another/dir\" to the path of searched directories\n" ++
            "    > :flags set src-dir=another/dir\n" ++
            "    1: src-dir=another/dir\n\n" ++
            "    List all set flags:\n" ++
            "    > :flags list\n" ++
            "    0: src-dir=some/dir\n" ++
            "    1: src-dir=another/dir\n\n" ++
            "    Remove \"some/dir\" from the path of searched directores\n" ++
            "    > :flags remove 0\n" ++
            "    0: src-dir=some/dir\n\n" ++
            "    Clear all flags\n" ++
            "    > :flags clear\n" ++
            "    All flags cleared"
-}            

helpInfo = "General usage directions: <https://github.com/evancz/elm-repl#elm-repl>\n\
           \Additional commands available from the prompt:\n\n\
           \  :help\t\t\tList available commands\n\
           \  :flags\t\tManipulate flags sent to elm compiler\n\
           \  :reset\t\tClears all previous imports\n\
           \  :exit\t\t\tExits elm-repl.\n"
