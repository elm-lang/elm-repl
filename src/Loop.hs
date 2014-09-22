module Loop (loop) where

import Control.Monad.Trans (lift, liftIO)
import System.Console.Haskeline (InputT, MonadException, Settings, getInputLine,
                                 handleInterrupt, runInputT, withInterrupt)
import System.Exit (ExitCode(ExitSuccess))

import qualified Command as Cmd
import qualified Environment as Env
import qualified Evaluator as Eval
import qualified Flags
import qualified Input
import Monad (ReplM, runReplM)
import qualified Parse


loop :: Flags.Flags -> Settings ReplM -> IO ExitCode
loop flags settings =
    runReplM flags initialEnv $ runInputT settings (withInterrupt acceptInput)
  where
    initialEnv =
        Env.empty (Flags.compiler flags) (Flags.interpreter flags)


acceptInput :: InputT ReplM ExitCode
acceptInput =
 do rawInput <- handleInterrupt (return (Just "")) getInput
    case rawInput of
      Nothing ->
        return ExitSuccess

      Just userInput ->
        do  let action = Parse.rawInput userInput
            result <- lift (handle action)
            case result of
              Just exit -> return exit
              Nothing   -> acceptInput


handle :: Input.Input -> ReplM (Maybe ExitCode)
handle action =
    case action of
      Input.Meta cmd ->
          Cmd.run cmd

      Input.Skip ->
          return Nothing

      Input.Code src ->
          do  handleInterrupt interruptedMsg (Eval.evalPrint src)
              return Nothing
    where
      interruptedMsg =
          liftIO $ putStrLn " Computation interrupted, any definitions were not completed."


getInput :: (MonadException m) => InputT m (Maybe String)
getInput =
    go "> " ""
  where
    go lineStart inputSoFar =
        do  input <- getInputLine lineStart
            case input of
              Nothing  -> return Nothing
              Just new -> continueWith (inputSoFar ++ new)

    continueWith inputSoFar =
        if null inputSoFar || last inputSoFar /= '\\'
            then return (Just inputSoFar)
            else go "| " (init inputSoFar ++ "\n")
