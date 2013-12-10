{-# LANGUAGE OverloadedStrings #-}
module Evaluator where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString as BS
import qualified Language.Elm as Elm
import qualified Environment as Env
import qualified Data.Set as Set
import Data.List

import System.IO.Error  (isDoesNotExistError)
import System.Directory (removeFile)
import System.Exit      (ExitCode(..))
import System.FilePath  ((</>), replaceExtension)
import System.Process
import Control.Exception
import Control.Monad (unless)

runRepl :: Env.Repl -> IO Bool
runRepl environment =
  do writeFile tempElm (Env.toElm environment)
     result <- run "elm" elmArgs $ \types -> do
       reformatJS tempJS
       run "node" nodeArgs $ \value' ->
           let value = BSC.init value'
               tipe = scrapeOutputType types
               isTooLong = BSC.isInfixOf "\n" value ||
                           BSC.isInfixOf "\n" tipe ||
                           BSC.length value + BSC.length tipe > 80   
               message = BS.concat [ if isTooLong then value' else value, tipe ]
           in  unless (BSC.null value') $ BSC.putStrLn message
     removeIfExists tempElm
     return result
  where
    tempElm = "repl-temp-000.elm"
    tempJS  = "build" </> replaceExtension tempElm "js"
    
    nodeArgs = [tempJS]
    elmArgs  = (getSrcs environment) ++ ["--make", "--only-js", "--print-types", tempElm]

    run name args nextComputation =
      let failure message = BSC.putStrLn message >> return False
          missingExe = unlines [ "Error: '" ++ name ++ "' command not found."
                               , "  Do you have it installed?"
                               , "  Can it be run from anywhere? I.e. is it on your PATH?" ]
      in
      do (_, stdout, _, handle') <- createProcess (proc name args) { std_out = CreatePipe }
         exitCode <- waitForProcess handle'
         case (exitCode, stdout) of
           (ExitFailure 127, _)      -> failure $ BSC.pack missingExe
           (_, Nothing)              -> failure "Unknown error!"
           (ExitFailure _, Just out) -> failure =<< BSC.hGetContents out
           (ExitSuccess  , Just out) ->
               do nextComputation =<< BS.hGetContents out
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
          , "if ('", Env.output, "' in repl)\n"
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
                        
getSrcs :: Env.Repl -> [String]
getSrcs env = map ("--src-dir=" ++ ) . Set.toList . Env.src_dirs $ env
  
  