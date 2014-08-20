{-# LANGUAGE OverloadedStrings #-}
module Evaluator where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (unless)
import Control.Monad.Cont (ContT(ContT, runContT))
import Control.Monad.RWS (get, modify)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString as BS
import qualified Data.Char as Char
import qualified Elm.Internal.Paths as Elm
import System.Directory (doesFileExist, removeFile)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.FilePath ((</>), (<.>))
import System.IO (hPutStrLn, stderr, stdout)
import System.Process (readProcessWithExitCode)

import Action (Term)
import qualified Environment as Env
import Monad (ReplM)

evalPrint :: Term -> ReplM ()
evalPrint term =
  do modify $ Env.insert term 
     env <- get
     liftIO $ writeFile tempElm $ Env.toElm env
     let elmArgs = Env.flags env ++ ["--make", "--only-js", "--print-types", tempElm]
     liftIO . runConts $
       do types  <- runCmd (Env.compilerPath env) elmArgs
          liftIO $ reformatJS tempJS
          value' <- runCmd (Env.interpreterPath env) nodeArgs
          let value = BSC.init value'
              tipe = scrapeOutputType types
              isTooLong = BSC.isInfixOf "\n" value ||
                          BSC.isInfixOf "\n" tipe ||
                          BSC.length value + BSC.length tipe > 80   
              message = BS.concat [ if isTooLong then value' else value, tipe ]
            in liftIO $ unless (BSC.null value') $ BSC.hPutStrLn stdout message
     liftIO $ removeIfExists tempElm
     return ()
  where

    runConts m = runContT m (\_ -> return ())
    
    tempElm = "repl-temp-000" <.> "elm"
    tempJS  = "build" </> "Repl" <.> "js"
    
    nodeArgs = [tempJS]

runCmd :: FilePath -> [String] -> ContT () IO BS.ByteString
runCmd name args = ContT $ \ret ->
  do (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode name args ""
     case exitCode of
       ExitSuccess -> ret (BSC.pack stdout)
       ExitFailure code
           | code == 127  -> failure missingExe  -- UNIX
           | code == 9009 -> failure missingExe  -- Windows
           | otherwise    -> failure (stdout ++ stderr)
  where
    failure message = liftIO $ hPutStrLn stderr message

    missingExe =
        unlines $
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
          , "var show = Elm.Native.Show.make(context).show;"
          , "if ('", Env.lastVar, "' in repl)\n"
          , "  console.log(show(repl.", Env.lastVar, "));" ]

scrapeOutputType :: BS.ByteString -> BS.ByteString
scrapeOutputType = dropName . squashSpace . takeType . dropWhile (not . isOut) . BSC.lines
  where isOut    = (||) <$> (BS.isPrefixOf Env.lastVar) <*> (BS.isPrefixOf (BS.append "Repl." Env.lastVar))
        dropName = BSC.cons ' ' . BSC.dropWhile (/= ':')
        takeType (n:rest) = n : takeWhile isMoreType rest
        takeType []       = error "Internal error in elm-repl (takeType): Please report this bug to https://github.com/elm-lang/elm-repl/issues/"
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
