module Monad (ReplM, runReplM) where

import Control.Monad.RWS (RWST, runRWST)
import qualified Environment as Env
import qualified Flags


-- Reader: Build Flags
-- State:  Current Environment
type ReplT = RWST Flags.Flags () Env.Env
type ReplM = ReplT IO


runReplM :: Flags.Flags -> Env.Env -> ReplM a -> IO a
runReplM flags env command =
    do  (x,_,_) <- runRWST command flags env
    	return x
