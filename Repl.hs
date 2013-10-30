{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Language.Elm as Elm
import System.IO
import System.FilePath
import System.Process
import System.Exit

import qualified Environment as Env

main = loop Env.empty

loop :: Env.Repl -> IO ()
loop env = do
  str <- getInput
  case str of
    "" -> loop env
    _  -> do
      let env' = Env.insert str env
      success <- runRepl env'
      loop (if success then env' else env)

getInput :: IO String
getInput = get "> " ""
    where
      get str old = do
        putStr str
        hFlush stdout
        new <- getLine
        continueWith (old ++ new)

      continueWith str
        | null str || last str /= '\\' = return str
        | otherwise = get "| " (init str ++ "\n")

runRepl :: Env.Repl -> IO Bool
runRepl env =
  do writeFile tempElm (Env.toElm env)
     onSuccess compile $ \_ -> do
       reformatJS tempJS
       onSuccess run putStr
  where
    tempElm = "repl-temp-000.elm"
    tempJS  = "build" </> replaceExtension tempElm "js"
    
    run = (proc "node" [tempJS]) { std_out = CreatePipe }
    compile = (proc "elm" args) { std_out = CreatePipe }
        where args = [ "--make", "--only-js", tempElm ]

    onSuccess action success =
      let failure message = putStrLn message >> return False in
      do (_, stdout, _, handle) <- createProcess action
         exitCode <- waitForProcess handle
         case (exitCode, stdout) of
           (ExitFailure 127, _)      -> failure "Error: elm binary not found in your path."
           (_, Nothing)              -> failure "Unknown error!"
           (ExitFailure _, Just out) -> failure =<< hGetContents out
           (ExitSuccess  , Just out) ->
               do success =<< hGetContents out
                  return True

reformatJS :: String -> IO ()
reformatJS tempJS =
  do rts <- BS.readFile =<< Elm.runtime
     src <- BS.readFile tempJS
     BS.length src `seq` BS.writeFile tempJS (BS.concat (rts:src:out))
  where
    out = [ "var context = { inputs:[] };\n"
          , "var repl = Elm.Repl.make(context);\n"
          , "console.log(context.Native.Show.values.show(repl.", Env.expr, "));" ]