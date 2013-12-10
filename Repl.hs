{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans
import System.Console.Haskeline
import qualified Evaluator as Eval
import qualified Environment as Env
import System.Exit

main :: IO ()
main = runInputT defaultSettings $ withInterrupt $ loop Env.empty

loop :: Env.Repl -> InputT IO ()
loop environment@(Env.Repl _ _ _ wasCtrlC) = do
  str' <- handleInterrupt (return Nothing) getInput
  case str' of
    Just input ->
        loop =<< liftIO (Eval.runRepl input $ environment {Env.ctrlc = False})

    Nothing
        | wasCtrlC  -> lift $ exitWith (ExitFailure 130)
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