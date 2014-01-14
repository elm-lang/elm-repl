{-# LANGUAGE OverloadedStrings #-}
module Evaluator where

import qualified Data.Char             as Char
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString       as BS
import qualified Elm.Internal.Paths    as Elm
import qualified Environment           as Env

import Control.Applicative ((<$>), (<*>))
import Control.Monad       (unless)
import Control.Monad.RWS   (get, modify, MonadState)
import Control.Monad.Trans (liftIO)
import System.Directory    (doesFileExist, removeFile)
import System.Exit         (ExitCode(..))
import System.FilePath     ((</>), replaceExtension)
import System.IO
import System.Process

import Monad

evalPrint :: String -> ReplM ()
evalPrint input | all Char.isSpace input = return ()
evalPrint input | otherwise =
  do modify $ Env.insert input
     env <- get
     liftIO $ writeFile tempElm $ Env.toElm env
     let elmArgs = Env.flags env ++ ["--make", "--only-js", "--print-types", tempElm]
     liftIO . runCmdWithCallback (Env.compilerPath env) elmArgs $ \types -> do
       reformatJS tempJS
       runCmdWithCallback "node" nodeArgs $ \value' ->
           let value = BSC.init value'
               tipe = scrapeOutputType types
               isTooLong = BSC.isInfixOf "\n" value ||
                           BSC.isInfixOf "\n" tipe ||
                           BSC.length value + BSC.length tipe > 80   
               message = BS.concat [ if isTooLong then value' else value, tipe ]
           in  unless (BSC.null value') $ BSC.hPutStrLn stdout message
     liftIO $ removeIfExists tempElm
     return ()
  where

    tempElm = "repl-temp-000.elm"
    tempJS  = "build" </> replaceExtension tempElm "js"
    
    nodeArgs = [tempJS]

runCmdWithCallback :: FilePath -> [String] -> (BS.ByteString -> IO ()) -> IO ()
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
 where failure message = BSC.hPutStrLn stderr message
       missingExe = BSC.pack $ unlines $
                    [ "Error: '" ++ name ++ "' command not found."
                    , "  Do you have it installed?"
                    , "  Can it be run from anywhere? I.e. is it on your PATH?" ]

reformatJS :: String -> IO ()
reformatJS tempJS =
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
          , "if ('", Env.lastVar, "' in repl)\n"
          , "  console.log(context.Native.Show.values.show(repl.", Env.lastVar, "));" ]

scrapeOutputType :: BS.ByteString -> BS.ByteString
scrapeOutputType = dropName . squashSpace . takeType . dropWhile (not . isOut) . BSC.lines
  where isOut    = (||) <$> (BS.isPrefixOf Env.lastVar) <*> (BS.isPrefixOf (BS.append "Repl." Env.lastVar))
        dropName = BSC.cons ' ' . BSC.dropWhile (/= ':')
        takeType (n:rest) = n : takeWhile isMoreType rest
        takeType []       = error "Internal error in elm-repl (takeType): Please report this bug to https://github.com/evancz/elm-repl/issues/"
        isMoreType = (&&) <$> not . BS.null <*> (Char.isSpace . BSC.head)
        squashSpace = BSC.unwords . BSC.words . BSC.unwords

freshLine :: BS.ByteString -> (BS.ByteString, BS.ByteString)
freshLine str | BS.null rest' = (line,"")
              | otherwise     = (line, BS.tail rest')
  where
    (line,rest') = BSC.break (=='\n') str

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = do
  exists <- doesFileExist fileName
  if exists
    then removeFile fileName
    else return ()
