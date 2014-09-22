module Eval.Command (Command, run) where

import Control.Monad.RWS (RWST, runRWST)
import qualified Environment as Env
import qualified Flags

{-| The evaluation Command handles the details of running the REPL. It uses
the Reader/Writer/State monad to keep track of some important details:

  * Reader: initial configuration
  * Writer: nothing
  * State:  current environment

-}
type Command =
	RWST Flags.Flags () Env.Env IO


run :: Flags.Flags -> Env.Env -> Command a -> IO a
run flags env command =
    do  (x,_,_) <- runRWST command flags env
    	return x
