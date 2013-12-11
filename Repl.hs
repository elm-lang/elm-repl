{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.Trans
import System.Console.Haskeline
import qualified Evaluator as Eval
import qualified Environment as Env
import System.Directory
import System.Exit

main :: IO ()
main = do
  buildExisted <- doesDirectoryExist "build"
  cacheExisted <- doesDirectoryExist "cache"
  exitCode <- runInputT defaultSettings $ withInterrupt $ loop Env.empty
  when (not buildExisted) (removeDirectoryRecursive "build")
  when (not cacheExisted) (removeDirectoryRecursive "cache")
  exitWith exitCode

loop :: Env.Repl -> InputT IO ExitCode
loop environment@(Env.Repl _ _ _ wasCtrlC) = do
  str' <- handleInterrupt (return Nothing) getInput
  case str' of
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