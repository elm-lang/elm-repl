module Command where

import Data.Functor             ((<$>), (<$))
import Control.Monad.Trans      (liftIO)
import Control.Monad.State      (get, modify)
import System.Exit              (ExitCode, exitSuccess)

import qualified Data.List   as List

import Action (Command(..))
import qualified Environment as Env
import Monad (ReplM)

run :: Command -> ReplM (Maybe ExitCode)
run cmd =
  case cmd of
    Exit         -> Just <$> liftIO exitSuccess
    Help m       -> displayErr "Bad command\n" m >> display helpInfo
    InfoFlags m  -> displayErr "Bad flag\n" m    >> display flagsInfo
    ListFlags    -> display . unlines . Env.flags =<< get

    AddFlag flag -> modifyIfPresent True flag "Added " "Flag already added!" $ \env ->
        env { Env.flags = Env.flags env ++ [flag] }

    RemoveFlag flag -> modifyIfPresent False flag "Removed flag " "No such flag." $ \env ->
      env {Env.flags = List.delete flag $ Env.flags env}

    Reset -> modifyAlways "Environment Reset" (Env.empty . Env.compilerPath)
    ClearFlags -> modifyAlways "All flags cleared" $ \env ->
      env {Env.flags = []}

  where display msg = Nothing <$ (liftIO . putStrLn $ msg)
        displayErr msg m = case m of
          Nothing -> return ()
          Just err -> liftIO . putStrLn $ (msg ++ err)
        modifyIfPresent b flag msgSuc msgFail mod = do
          env <- get
          if not b `xor` (flag `elem` Env.flags env)
            then display msgFail
            else Nothing <$ do
          liftIO . putStrLn $ msgSuc ++ flag
          modify mod
        modifyAlways msg mod = Nothing <$ do
          liftIO . putStrLn $ msg
          modify mod

xor :: Bool -> Bool -> Bool
xor True  = not
xor False = id

flagsInfo :: String
flagsInfo = "Usage: flags [operation]\n\
            \\n\
            \  operations:\n\
            \    add --src-dir=FILEPATH\tAdd a compiler flag\n\
            \    remove --src-dir=FILEPATH\tRemove a compiler flag\n\
            \    list\t\t\tList all flags that have been added\n\
            \    clear\t\t\tClears all flags\n" 

helpInfo :: String
helpInfo = "General usage directions: <https://github.com/elm-lang/elm-repl#elm-repl>\n\
           \Additional commands available from the prompt:\n\
           \\n\
           \  :help\t\t\tList available commands\n\
           \  :flags\t\tManipulate flags sent to elm compiler\n\
           \  :reset\t\tClears all previous imports\n\
           \  :exit\t\t\tExits elm-repl\n"
