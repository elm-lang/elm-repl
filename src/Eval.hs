module Eval (eval) where

import Control.Monad.Trans (liftIO)
import System.Console.Haskeline (handleInterrupt)
import qualified System.Exit as Exit

import qualified Eval.Code as Code
import qualified Eval.Command as Eval
import qualified Eval.Meta as Meta
import qualified Input


eval :: Input.Input -> Eval.Command (Maybe Exit.ExitCode)
eval action =
    case action of
      Input.Meta cmd ->
          Meta.eval cmd

      Input.Skip ->
          return Nothing

      Input.Code code ->
          do  handleInterrupt interruptedMsg (Code.eval code)
              return Nothing
    where
      interruptedMsg =
          liftIO $ putStrLn " Computation interrupted, any definitions were not completed."
