{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Language.Elm as Elm
import System.IO
import System.FilePath
import System.Process
import System.Exit

main = loop [] Map.empty

loop :: [String] -> Map.Map String String -> IO ()
loop history env = do
  expr <- getExpression
  case expr of
    "" -> loop history env
    _  -> do
      let prev = case history of { it:_ -> [("it",it)] ; _ -> [] }
          env' = foldr (uncurry Map.insert) env $ ("it_000",expr) : prev
      success <- runRepl env'
      if success then loop (expr:history) env'
                 else loop history env

getExpression = get "> " ""
    where
      get str old = do
        putStr str
        hFlush stdout
        new <- getLine
        continueWith (old ++ new)

      continueWith expr
        | null expr || last expr /= '\\' = return expr
        | otherwise = get "| " (init expr ++ "\n")

runRepl env =
  do writeFile tempElm (toElm env)
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

toElm env = unlines $ "module Repl where" : map toDecl (Map.toList env)
    where
      toDecl (name,expr) =
          name ++ " =" ++ concatMap ("\n  "++) (lines expr)

reformatJS tempJS =
  do rts <- BS.readFile =<< Elm.runtime
     src <- BS.readFile tempJS
     BS.length src `seq` BS.writeFile tempJS (BS.concat [rts,src,out])
  where
    out = "var context = { inputs:[] };\n\
          \var repl = Elm.Repl.make(context);\n\
          \console.log(context.Native.Show.values.show(repl.it_000));"