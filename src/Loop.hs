module Loop (loop) where

import Control.Monad.Trans (lift)
import System.Console.Haskeline (InputT, MonadException, Settings, getInputLine,
                                 handleInterrupt, runInputT, withInterrupt)
import System.Exit (ExitCode(ExitSuccess))

import qualified Environment as Env
import qualified Eval
import qualified Flags
import qualified Read


loop :: Flags.Flags -> Settings Env.Task -> IO ExitCode
loop flags settings =
    Env.run flags initialEnv $ runInputT settings (withInterrupt acceptInput)
  where
    initialEnv =
        Env.empty (Flags.compiler flags) (Flags.interpreter flags)


acceptInput :: InputT Env.Task ExitCode
acceptInput =
 do rawInput <- handleInterrupt (return (Just "")) getInput
    case rawInput of
      Nothing ->
        return ExitSuccess

      Just string ->
        do  let input = Read.input string
            result <- lift (Eval.eval input)
            case result of
              Just exit -> return exit
              Nothing   -> acceptInput


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
