module Eval (eval) where

import Control.Monad.Trans (liftIO)
import System.Console.Haskeline (handleInterrupt)
import qualified System.Exit as Exit

import qualified Environment as Env
import qualified Eval.Code as Code
import qualified Eval.Meta as Meta


eval :: Env.Input -> Env.Task (Maybe Exit.ExitCode)
eval action =
    case action of
      Env.Meta cmd ->
          Meta.eval cmd

      Env.Skip ->
          return Nothing

      Env.Code code ->
          do  handleInterrupt interruptedMsg (Code.eval code)
              return Nothing
    where
      interruptedMsg =
          liftIO $ putStrLn " Computation interrupted, any definitions were not completed."
