module Loop (loop) where

import Control.Monad.Trans (lift)
import System.Console.Haskeline
  ( InputT, MonadException, Settings, getInputLine
  , handleInterrupt, runInputT, withInterrupt
  )
import System.Exit (ExitCode(ExitSuccess))

import qualified Environment as Env
import qualified Eval
import qualified Read


loop :: Env.Env -> Settings Env.Task -> IO ExitCode
loop env settings =
    Env.run env $ runInputT settings (withInterrupt acceptInput)


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
