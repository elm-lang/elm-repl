{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans
import System.Console.Haskeline
import qualified Evaluator as Eval
import qualified Environment as Env
import Control.Monad (unless)

main :: IO ()
main = runInputT defaultSettings $ withInterrupt $ loop Env.empty

handleCtrlC :: Env.Repl -> InputT IO ()
handleCtrlC env@(Env.Repl _ _ _ wasCtrlC) = 
  unless wasCtrlC $ liftIO (putStrLn "(Ctrl-C again to quit)") >> loop (env {Env.ctrlc = True})


loop :: Env.Repl -> InputT IO ()
loop environment = handleInterrupt (handleCtrlC environment) $ do
  str <- getInput
  let env = environment {Env.ctrlc = False}
  case str of
    "" -> loop env
    _  -> do
      let env' = Env.insert str env
      success <- liftIO $ Eval.runRepl env'
      loop (if success then env' else env)

getInput :: InputT IO String
getInput = get "> " ""
    where
      get str old = do
        input <- getInputLine str
        case input of
          Nothing  -> return old
          Just new -> continueWith (old ++ new)

      continueWith str
        | null str || last str /= '\\' = return str
        | otherwise = get "| " (init str ++ "\n")