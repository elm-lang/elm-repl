module Monad (ReplM, runReplM)
       where

import Control.Monad.RWS (RWST, runRWST)
import Data.Functor ((<$>))

import qualified Environment as Env
import Flags (Flags)

-- Reader: Build Flags
-- State:  Current Environment
type ReplT = RWST Flags () Env.Repl
type ReplM = ReplT IO

runReplM :: Flags -> Env.Repl -> ReplM a -> IO a
runReplM fs repl m = (\(x,_,_) -> x) <$> runRWST m fs repl
