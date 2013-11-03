{-# LANGUAGE OverloadedStrings #-}
module Evaluator where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString as BS
-- import qualified Data.Map as Map
import qualified Language.Elm as Elm
import qualified Environment as Env

import System.IO.Error  (isDoesNotExistError)
import System.Directory (removeFile)
import System.Exit      (ExitCode(..))
import System.FilePath  ((</>), replaceExtension)
import System.Process
import Control.Exception


runRepl :: Env.Repl -> IO Bool
runRepl environment =
  do writeFile tempElm (Env.toElm environment)
     result <- onSuccess compile $ \types -> do
       reformatJS tempJS
       onSuccess run $ \value' ->
           let value = BSC.init value'
               tipe = scrapeOutputType types
               isTooLong = or [ BSC.isInfixOf "\n" value
                              , BSC.isInfixOf "\n" tipe
                              , BSC.length value + BSC.length tipe > 80 ]    
               message = BS.concat [ if isTooLong then value' else value, tipe ]
           in  if BSC.null value' then return () else BSC.putStrLn message
     removeIfExists tempElm
     return result
  where
    tempElm = "repl-temp-000.elm"
    tempJS  = "build" </> replaceExtension tempElm "js"
    
    run = (proc "node" [tempJS]) { std_out = CreatePipe }
    compile = (proc "elm" args) { std_out = CreatePipe }
        where args = [ "--make", "--only-js", "--print-types", tempElm ]

    onSuccess action success =
      let failure message = BSC.putStrLn message >> return False in
      do (_, stdout, _, handle') <- createProcess action
         exitCode <- waitForProcess handle'
         case (exitCode, stdout) of
           (ExitFailure 127, _)      -> failure "Error: elm binary not found in your path."
           (_, Nothing)              -> failure "Unknown error!"
           (ExitFailure _, Just out) -> failure =<< BS.hGetContents out
           (ExitSuccess  , Just out) ->
               do _ <- success =<< BS.hGetContents out
                  return True

reformatJS :: String -> IO ()
reformatJS tempJS =
  do rts <- BS.readFile =<< Elm.runtime
     src <- BS.readFile tempJS
     BS.length src `seq` BS.writeFile tempJS (BS.concat [rts,src,out])
  where
    out = BS.concat
          [ "var context = { inputs:[] };\n"
          , "var repl = Elm.Repl.make(context);\n"
          , "if (repl.", Env.output, ")\n"
          , "  console.log(context.Native.Show.values.show(repl.", Env.output, "));" ]

scrapeOutputType :: BS.ByteString -> BS.ByteString
scrapeOutputType types
    | name == Env.output = tipe
    | BS.null rest       = ""
    | otherwise          = scrapeOutputType rest
    where
      (next,rest) = freshLine types
      (name,tipe) = BSC.splitAt (BSC.length Env.output) next

      freshLine str
          | BSC.take 2 rest' == "\n " = (BS.append line line', rest'')
          | BS.null rest' = (line,"")
          | otherwise    = (line, BS.tail rest')
          where
            (line,rest') = BSC.break (=='\n') str
            (line',rest'') = freshLine rest'

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `Control.Exception.catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e