{-# LANGUAGE OverloadedStrings #-}
module Evaluator where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString as BS
import qualified Language.Elm as Elm
import qualified Environment as Env

import System.IO
import System.IO.Error  (isDoesNotExistError)
import System.Directory (removeFile)
import System.Exit      (ExitCode(..))
import System.FilePath  ((</>), replaceExtension)
import System.Process
import Control.Exception
import Control.Monad (unless)

runRepl :: String -> Env.Repl -> IO Env.Repl
runRepl "" env = return env
runRepl input oldEnv =
  do writeFile tempElm $ Env.toElm newEnv
     success <- run "elm" elmArgs $ \types -> do
       reformatJS input tempJS
       run "node" nodeArgs $ \value' ->
           let value = BSC.init value'
               tipe = scrapeOutputType types
               isTooLong = BSC.isInfixOf "\n" value ||
                           BSC.isInfixOf "\n" tipe ||
                           BSC.length value + BSC.length tipe > 80   
               message = BS.concat [ if isTooLong then value' else value, tipe ]
           in  do unless (BSC.null value') $ BSC.hPutStrLn stdout message
                  return True
     removeIfExists tempElm
     return $ if success then newEnv else oldEnv
  where
    newEnv = Env.insert input oldEnv

    tempElm = "repl-temp-000.elm"
    tempJS  = "build" </> replaceExtension tempElm "js"
    
    nodeArgs = [tempJS]
    elmArgs  = ["--make", "--only-js", "--print-types", tempElm]

    run name args nextComputation =
      do (_, stdout, stderr, handle') <-
             createProcess (proc name args) { std_out = CreatePipe
                                            , std_err = CreatePipe }
         exitCode <- waitForProcess handle'
         case (exitCode, stdout, stderr) of
           (ExitSuccess, Just out, Just _) ->
               nextComputation =<< BS.hGetContents out
           (ExitFailure 127, Just _, Just _) -> failure missingExe
           (ExitFailure _, Just out, Just err) -> do e <- BSC.hGetContents err
                                                     o <- BSC.hGetContents out
                                                     failure (BS.concat [o,e])
           (_, _, _) -> failure "Unknown error!"
      where
        failure message = BSC.hPutStrLn stderr message >> return False
        missingExe = BSC.pack $ unlines $
                     [ "Error: '" ++ name ++ "' command not found."
                     , "  Do you have it installed?"
                     , "  Can it be run from anywhere? I.e. is it on your PATH?" ]


reformatJS :: String -> String -> IO ()
reformatJS input tempJS =
  do rts <- BS.readFile =<< Elm.runtime
     src <- BS.readFile tempJS
     BS.length src `seq` BS.writeFile tempJS (BS.concat [rts,src,out])
  where
    out = BS.concat
          [ "process.on('uncaughtException', function(err) {\n"
          , "  var input = '", BSC.pack input, "';\n"
          , "  var msg = (input.slice(0,7) === 'import ') ? ", badImport, " : ('Runtime error: ' + err);\n"
          , "  process.stderr.write(msg);\n"
          , "  process.exit(1);\n"
          , "});\n"
          , "var context = { inputs:[] };\n"
          , "var repl = Elm.Repl.make(context);\n"
          , "if ('", Env.output, "' in repl)\n"
          , "  console.log(context.Native.Show.values.show(repl.", Env.output, "));" ]

    badImport = "('Error: unable to import \\\"' + input.slice(7).replace(/ /g,'') + '\\\".\\nIt may rely on a browser API that is unavailable on the command line.')"


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