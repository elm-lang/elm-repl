module Repl (run) where

import Control.Monad.Trans
import System.Console.Haskeline hiding (handle)
import System.Exit

import Monad

import qualified Action      as Act
import qualified Command     as Cmd
import qualified Evaluator   as Eval
import qualified Environment as Env
import qualified Flags
import qualified Parse

run :: Flags.Flags -> Settings ReplM -> IO ExitCode
run flags settings =
    runReplM flags initialEnv . runInputT settings $ withInterrupt repl
  where
    initialEnv = Env.empty (Flags.compiler flags)

repl :: InputT ReplM ExitCode
repl = do
  str' <- handleInterrupt (return $ Just "") getInput
  case str' of
    Nothing -> return ExitSuccess
    Just str -> do
      let action = Parse.input str
      result <- lift $ handle action
      case result of
        Just exit -> return exit
        Nothing   -> repl

handle :: Act.Action -> ReplM (Maybe ExitCode)
handle action =
    case action of
      Act.Command cmd -> Cmd.run cmd

      Act.Skip -> return Nothing

      Act.Code src ->
          do Eval.evalPrint src
             return Nothing

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
