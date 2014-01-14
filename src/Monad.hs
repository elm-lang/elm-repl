module Monad (ReplM, runReplM)
       where

import Data.Functor ((<$>))
import Control.Monad.RWS

import Flags (Flags)
import qualified Environment as Env

-- Reader: Build Flags
-- State:  Current Environment
type ReplT = RWST Flags () Env.Repl
type ReplM = ReplT IO

runReplM :: Flags -> Env.Repl -> ReplM a -> IO a
runReplM fs repl m = (\(x,_,_) -> x) <$> runRWST m fs repl
