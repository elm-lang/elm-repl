{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans
import System.Console.Haskeline
import qualified Evaluator as Eval
import qualified Environment as Env
import System.Exit

import Text.Parsec hiding(getInput)
import qualified Data.Set as Set

data Command
 = AddSrc String 
 | RemoveSrc String
 | ListSrc
 | ClearSrc
   deriving Show

main :: IO ()
main = runInputT defaultSettings $ withInterrupt $ loop Env.empty

loop :: Env.Repl -> InputT IO ()
loop environment@(Env.Repl _ _ _ wasCtrlC _) = do
  str' <- handleInterrupt (return Nothing) getInput
  let env = environment {Env.ctrlc = False}
  case str' of
    Nothing -> if wasCtrlC then lift $ exitWith (ExitFailure 130)
                          else do 
                            lift $ putStrLn "(Ctrl-C again to exit)"
                            loop $ environment {Env.ctrlc = True}
    Just "" -> loop env
    Just (':':command) -> runCommand env command
    Just str  -> do
      let env' = Env.insert str env
      success <- liftIO $ Eval.runRepl env'
      loop (if success then env' else env)
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
                      
      
runCommand :: Env.Repl -> String -> InputT IO ()
runCommand env command = 
  case parseCommand command of
    Left err -> do
      lift . putStrLn $ "Could not parse command '" ++ command ++ "':"
      lift . putStrLn . show $ err
      loop env
    Right command -> do
      let (env', sideEffects) = 
            case command of
              AddSrc path -> (env { Env.src_dirs = Set.insert path (Env.src_dirs env) }, return ())
              RemoveSrc path -> (env { Env.src_dirs = Set.delete path (Env.src_dirs env) }, return ())
              ClearSrc -> (env { Env.src_dirs = Set.empty }, return ())
              ListSrc -> (env, mapM_ putStrLn . Set.toList $ (Env.src_dirs env))
      lift $ sideEffects
      loop env'

parseCommand :: String -> Either ParseError Command
parseCommand str = parse commands "" str

commands = try addSrc <|> removeSrc <|> listSrc <|> clearSrc

addSrc = do
  _ <- string "src-dir="
  path <- manyTill anyChar eof
  return (AddSrc path)
  
removeSrc = do  
  _ <- string "rm-src-dir="
  path <- manyTill anyChar eof
  return (RemoveSrc path)
  
listSrc = do
  _ <- string "src-dir"
  _ <- spaces
  _ <- eof
  return ListSrc
  
clearSrc = do
  _ <- string "clear-src-dir"
  _ <- spaces
  _ <- eof
  return ClearSrc