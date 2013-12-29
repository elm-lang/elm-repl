module Main where

import Control.Monad
import Control.Monad.Trans
import Data.Functor ((<$>))
import System.Console.Haskeline
import qualified Evaluator as Eval
import qualified Environment as Env
import qualified System.Console.CmdArgs as CmdArgs
import System.Directory
import System.Environment
import System.Exit
import System.FilePath ((</>))

import Text.Parsec hiding (getInput)
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Flags

data Command
    = AddFlag String
    | RemoveFlag String
    | ListFlags
    | ClearFlags
    | InfoFlags
    | Help
    | Exit
    | Reset
      deriving Show

welcomeMessage :: String
welcomeMessage = "Elm REPL <https://github.com/evancz/elm-repl#elm-repl>\n\
                 \Type :help for help, :exit to exit"

elmdir :: IO FilePath
elmdir = do
  dir <- (</> "repl") <$> getAppUserDataDirectory "elm"
  createDirectoryIfMissing True dir
  return dir

mkSettings :: (MonadIO m) => IO (Settings m)
mkSettings = do
  historyFile <- (</> "history") <$> elmdir
  return $ defaultSettings { historyFile = Just historyFile }

main :: IO ()
main = do
  flags <- CmdArgs.cmdArgs Flags.flags
  buildExisted <- doesDirectoryExist "build"
  cacheExisted <- doesDirectoryExist "cache"
  settings     <- mkSettings
  putStrLn welcomeMessage
  exitCode <- runInputT settings $ withInterrupt $ loop (Env.empty (Flags.compiler flags))
  when (not buildExisted) (removeDirectoryRecursive "build")
  when (not cacheExisted) (removeDirectoryRecursive "cache")
  exitWith exitCode

loop :: Env.Repl -> InputT IO ExitCode
loop environment = do
  str' <- handleInterrupt (return . Just $ "") getInput
  case str' of
    Just (':':command) -> runCommand environment command
    Just input         -> loop =<< liftIO (Eval.runRepl input environment)
    Nothing            -> return ExitSuccess

getInput :: InputT IO (Maybe String)
getInput = get "> " ""
    where
      get str old = do
        input <- getInputLine str
        case input of
          Nothing  -> return Nothing
          Just new -> continueWith (old ++ new)

      continueWith str
        | null str || last str /= '\\' = return $ Just str
        | otherwise = get "| " (init str ++ "\n")
                      
runCommand :: Env.Repl -> String -> InputT IO ExitCode
runCommand env raw = 
  case parse commands "" raw of
    Left err -> do
      lift . putStrLn $ "Could not parse command '" ++ raw ++ "':"
      lift . putStrLn $ show err
      loop env
    Right command -> handleCommand env command

handleCommand :: Env.Repl -> Command -> InputT IO ExitCode
handleCommand env command =
    let loop' (msg,env') = liftIO (putStrLn msg) >> loop env' in
    case command of
      Exit  -> liftIO exitSuccess

      Reset -> loop' ("Environment Reset", Env.empty (Env.compilerPath env))

      Help  -> loop' (helpInfo, env)

      AddFlag flag ->
          if flag `elem` Env.flags env
          then loop' ( "Flag already added!", env )
          else loop' ( "Added " ++ flag
                     , env { Env.flags = Env.flags env ++ [flag] } )

      RemoveFlag flag ->
          if flag `notElem` Env.flags env
          then loop' ( "No such flag.", env )
          else loop' ( "Removed flag " ++ flag
                     , env {Env.flags = List.delete flag $ Env.flags env} )

      ListFlags  -> loop' (List.intercalate "\n" $ Env.flags env, env)

      ClearFlags -> loop' ("All flags cleared", env {Env.flags = []})

      InfoFlags  -> loop' (flagsInfo, env)

commands :: Parsec String () Command
commands = choice (flags : basics)
    where
      basics = map basicCommand [ ("exit",Exit), ("reset",Reset), ("help",Help) ]

      basicCommand (name,command) =
          string name >> spaces >> eof >> return command

flags :: Parsec String () Command
flags = do
  string "flags"
  many space
  choice [ do string "add" >> many1 space >> srcDir
         , do string "remove" >> many1 space
              n <- many1 digit
              return $ RemoveFlag (read n)
         , do string "list"
              return ListFlags
         , do string "clear"
              return ClearFlags
         , return InfoFlags
         ]

srcDir :: Parsec String () Command
srcDir = do
  string "--src-dir="
  dir <- manyTill anyChar (choice [ space >> return (), eof ])
  return $ AddFlag $ "--src-dir=" ++ dir

flagsInfo :: String
flagsInfo = "Usage: flags [operation]\n\
            \\n\
            \  operations:\n\
            \    add --src-dir=FILEPATH\tAdd a compiler flag\n\
            \    remove --src-dir=FILEPATH\tRemove a compiler flag\n\
            \    list\t\t\tList all flags that have been added\n\
            \    clear\t\t\tClears all flags\n" 

helpInfo :: String
helpInfo = "General usage directions: <https://github.com/evancz/elm-repl#elm-repl>\n\
           \Additional commands available from the prompt:\n\
           \\n\
           \  :help\t\t\tList available commands\n\
           \  :flags\t\tManipulate flags sent to elm compiler\n\
           \  :reset\t\tClears all previous imports\n\
           \  :exit\t\t\tExits elm-repl\n"
