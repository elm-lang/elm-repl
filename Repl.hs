{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.Trans
import System.Console.Haskeline
import qualified Evaluator as Eval
import qualified Environment as Env
import System.Environment
import System.Directory
import System.Exit

import Text.Parsec hiding(getInput)
import qualified Data.Map as Map

data Command
 = AddFlag Env.Flag
 | RemoveFlag Env.FlagKey
 | ListFlags
 | ClearFlags
 | InfoFlags
 | Help
 | Quit
 | Reset

version = "elm-repl, version 0.1.0.1: https://github.com/evancz/elm-repl"

welcomeMessage = version ++ "   type :help for help"

displayHelp = putStrLn $ 
              version ++ "\n\n" ++
              "Usage: elm-repl [OPTIONS]\n\n" ++
              "Flags:\n" ++
              "  --compiler=PATH\tSpecify the compiler to use when evaluating statements.\n" ++
              "  --help\t\tDisplay help message."

main :: IO ()
main = do
  helpFlag <- getArgs >>= return . getHelpFlag
  case helpFlag of
    True -> displayHelp
    _ -> do
      buildExisted <- doesDirectoryExist "build"
      cacheExisted <- doesDirectoryExist "cache"
      compilerPath <- getArgs >>= return . getCompilerPath
      putStrLn welcomeMessage
      exitCode <- runInputT defaultSettings $ withInterrupt $ loop (Env.empty compilerPath)
      when (not buildExisted) (removeDirectoryRecursive "build")
      when (not cacheExisted) (removeDirectoryRecursive "cache")
      exitWith exitCode

getHelpFlag :: [String] -> Bool
getHelpFlag [] = False
getHelpFlag (x:xs) =
  let parsed = parse helpFlag "" x in
  case parsed of
    Left _ -> getHelpFlag xs
    Right _ -> True
    
helpFlag = do
  _ <- string "--help"
  return True

getCompilerPath :: [String] -> FilePath
getCompilerPath [] = "elm"
getCompilerPath (x:xs) = 
  let parsed = parse compilerPath "" x in
  case parsed of
    Left _ -> getCompilerPath xs
    Right path -> path

compilerPath = do
  _ <- string "--compiler="
  path <- manyTill anyChar endOfInput
  return path

loop :: Env.Repl -> InputT IO ExitCode
loop environment@(Env.Repl _ _ _ _ wasCtrlC _) = do
  str' <- handleInterrupt (return Nothing) getInput
  case str' of
    Just (':':command) -> runCommand environment command
    Just input ->
        loop =<< liftIO (Eval.runRepl input $ environment {Env.ctrlc = False})

    Nothing
        | wasCtrlC  -> return (ExitFailure 130)
        | otherwise -> do 
              lift $ putStrLn "(Ctrl-C again to exit)"
              loop $ environment {Env.ctrlc = True}

getInput :: InputT IO (Maybe String)
getInput = get "> " ""
    where
      get str old = do
        input <- getInputLine str
        case input of
          Nothing  -> return $ Just old
          Just new -> continueWith (old ++ new)

      continueWith str
        | null str || last str /= '\\' = return $ Just str
        | otherwise = get "| " (init str ++ "\n")
                      
runCommand :: Env.Repl -> String -> InputT IO ExitCode
runCommand env command = 
  case parseCommand command of
    Left err -> do
      lift . putStrLn $ "Could not parse command '" ++ command ++ "':"
      lift . putStrLn . show $ err
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
              Quit -> (env, exitSuccess)
              Reset -> (Env.empty $ Env.compilerPath env, none)
              Help -> (env, putStrLn helpInfo)
      lift $ sideEffects
      loop env'

none = return ()

parseCommand :: String -> Either ParseError Command
parseCommand str = parse commands "" str

commands = flags <|> reset <|> quit <|> help

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
  
quit = basicCommand "quit" Quit
  
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

helpInfo = "Commands available from the prompt:\n\n" ++
           "  <statement>\t\tevaluate <statement>\n" ++
           "  :flags\t\tManipulate flags sent to elm compiler\n" ++
           "  :help\t\t\tList available commands\n" ++
           "  :reset\t\tClears all previous imports\n" ++
           "  :quit\t\t\tExits elm-repl.\n"