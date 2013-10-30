{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans
import System.Console.Haskeline
import qualified Evaluator as Eval
import qualified Environment as Env

main :: IO ()
main = runInputT defaultSettings (loop Env.empty)

loop :: Env.Repl -> InputT IO ()
loop env = do
  str <- getInput
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