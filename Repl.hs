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
  let env = environment {Env.ctrlc = False}
  case str' of
    Nothing -> if wasCtrlC then lift $ exitWith (ExitFailure 130)
                          else do 
                            lift $ putStrLn "(Ctrl-C again to exit)"
                            loop $ environment {Env.ctrlc = True}
    Just "" -> loop env
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