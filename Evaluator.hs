{-# LANGUAGE OverloadedStrings #-}
module Evaluator where

import qualified Data.Char             as Char
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString       as BS
import qualified Elm.Internal.Paths    as Elm
import qualified Environment           as Env
import qualified Data.Map              as Map

import Control.Applicative ((<$>), (<*>))
import Control.Exception
import Control.Monad       (unless)
import System.Directory    (removeFile)
import System.Exit         (ExitCode(..))
import System.FilePath     ((</>), replaceExtension)
import System.IO
import System.IO.Error     (isDoesNotExistError)
import System.Process

runRepl :: String -> Env.Repl -> IO Env.Repl
runRepl "" env = return env
runRepl input oldEnv =
  do writeFile tempElm $ Env.toElm newEnv
     success <- runCmdWithCallback (Env.compilerPath newEnv) elmArgs $ \types -> do
       reformatJS input tempJS
       runCmdWithCallback "node" nodeArgs $ \value' ->
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
    elmArgs  = (getSrcs newEnv) ++ ["--make", "--only-js", "--print-types", tempElm]

runCmdWithCallback :: FilePath -> [String] -> (BS.ByteString -> IO Bool) -> IO Bool
runCmdWithCallback name args callback = do
  (_, stdout, stderr, handle') <- createProcess (proc name args) { std_out = CreatePipe
                                                                 , std_err = CreatePipe}
  exitCode <- waitForProcess handle'
  case (exitCode, stdout, stderr) of
    (ExitSuccess, Just out, Just _) ->
       callback =<< BS.hGetContents out
    (ExitFailure 127, Just _, Just _) -> failure missingExe
    (ExitFailure _, Just out, Just err) -> do
      e <- BSC.hGetContents err
      o <- BSC.hGetContents out
      failure (BS.concat [o,e])
    (_, _, _) -> failure "Unknown error!"
 where failure message = BSC.hPutStrLn stderr message >> return False
       missingExe = BSC.pack $ unlines $
                    [ "Error: '" ++ name ++ "' command not found."
                    , "  Do you have it installed?"
                    , "  Can it be run from anywhere? I.e. is it on your PATH?" ]

reformatJS :: String -> String -> IO ()
reformatJS input tempJS =
  do rts <- BS.readFile Elm.runtime
     src <- BS.readFile tempJS
     BS.length src `seq` BS.writeFile tempJS (BS.concat [rts,src,out])
  where
    out = BS.concat
          [ "process.on('uncaughtException', function(err) {\n"
          , "  process.stderr.write(err.toString());\n"
          , "  process.exit(1);\n"
          , "});\n"
          , "var document = document || {};"
          , "var window = window || {};"
          , "var context = { inputs:[], addListener:function(){}, node:{} };\n"
          , "var repl = Elm.Repl.make(context);\n"
          , "if ('", Env.output, "' in repl)\n"
          , "  console.log(context.Native.Show.values.show(repl.", Env.output, "));" ]

scrapeOutputType :: BS.ByteString -> BS.ByteString
scrapeOutputType = dropName . squashSpace . takeType . dropWhile (not . isOut) . BSC.lines
  where isOut    = BS.isPrefixOf Env.output
        dropName = BS.drop $ BSC.length Env.output
        takeType (n:rest) = n : takeWhile isMoreType rest
        isMoreType = (&&) <$> not . BS.null <*> (Char.isSpace . BSC.head)
        squashSpace = BSC.unwords . BSC.words . BSC.unwords

freshLine :: BS.ByteString -> (BS.ByteString, BS.ByteString)
freshLine str | BS.null rest' = (line,"")
              | otherwise     = (line, BS.tail rest')
  where
    (line,rest') = BSC.break (=='\n') str

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `Control.Exception.catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e
                        
getSrcs :: Env.Repl -> [String]
getSrcs env = 
  let srcDirs = map (getFlag . snd) . Map.toList . Env.flags $ env in
  case (Env.rootDirectory env) of
       Nothing -> srcDirs
       Just root -> ("--src-dir=" ++ root) : srcDirs

getFlag :: Env.Flag -> String
getFlag (p, v) = "--" ++ p ++ "=" ++ v
