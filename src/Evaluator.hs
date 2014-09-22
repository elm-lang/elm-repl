{-# LANGUAGE OverloadedStrings #-}
module Evaluator where

import Control.Monad.Cont (ContT(ContT, runContT))
import Control.Monad.RWS (get, modify)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString as BS
import qualified Data.Char as Char
import qualified Elm.Internal.Paths as Elm
import System.Directory (doesFileExist, removeFile)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.FilePath ((</>), (<.>), replaceExtension)
import System.IO (hPutStrLn, stderr, stdout)
import System.Process (readProcessWithExitCode)

import qualified Environment as Env
import qualified Input
import Monad (ReplM)

evalPrint :: (Maybe Input.DefName, String) -> ReplM ()
evalPrint code =
 do modify $ Env.insert code 
    env <- get
    liftIO $ writeFile tempElmPath (Env.toElmCode env)
    liftIO . runConts $ do
        types <- runCmd (Env.compilerPath env) (Env.flags env ++ elmArgs)
        liftIO $ reformatJS tempJsPath
        value <- runCmd (Env.interpreterPath env) [tempJsPath]
        liftIO $ printIfNeeded value (scrapeOutputType types)
    liftIO $ removeIfExists tempElmPath
    return ()
  where
    runConts m = runContT m (\_ -> return ())
    
    tempElmPath =
        "repl-temp-000" <.> "elm"

    tempJsPath =
        "build" </> replaceExtension tempElmPath "js"
    
    elmArgs =
        [ "--make"
        , "--only-js"
        , "--print-types"
        , tempElmPath
        ]

printIfNeeded :: BS.ByteString -> BS.ByteString -> IO ()
printIfNeeded rawValue tipe =
    if BSC.null rawValue
      then return ()
      else BSC.hPutStrLn stdout message
  where
    value = BSC.init rawValue

    isTooLong =
        BSC.isInfixOf "\n" value
        || BSC.isInfixOf "\n" tipe
        || BSC.length value + BSC.length tipe > 80

    message =
        BS.concat
            [ if isTooLong then rawValue else value
            , tipe
            ]


runCmd :: FilePath -> [String] -> ContT () IO BS.ByteString
runCmd name args = ContT $ \ret ->
  do  (exitCode, stdout, stderr) <-
          liftIO $ readProcessWithExitCode name args ""
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
reformatJS tempJsPath =
  do  rts <- BS.readFile Elm.runtime
      src <- BS.readFile tempJsPath
      BS.length src `seq` BS.writeFile tempJsPath (BS.concat [rts,src,out])
  where
    out =
        BS.concat
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
            , "  console.log(show(repl.", Env.lastVar, "));"
            ]


scrapeOutputType :: BS.ByteString -> BS.ByteString
scrapeOutputType rawTypeDump =
    dropName (squashSpace relevantLines)
  where
    squashSpace :: [BS.ByteString] -> BS.ByteString
    squashSpace multiLineTypeDecl =
        BSC.unwords (BSC.words (BSC.unwords multiLineTypeDecl))

    dropName :: BS.ByteString -> BS.ByteString
    dropName typeDecl =
        BSC.cons ' ' (BSC.dropWhile (/= ':') typeDecl)

    relevantLines :: [BS.ByteString]
    relevantLines =
        takeType . dropWhile (not . isLastVar) $ BSC.lines rawTypeDump

    isLastVar :: BS.ByteString -> Bool
    isLastVar line =
        BS.isPrefixOf Env.lastVar line
        || BS.isPrefixOf (BS.append "Repl." Env.lastVar) line

    takeType :: [BS.ByteString] -> [BS.ByteString]
    takeType lines =
        case lines of
          [] -> error errorMessage
          line : rest ->
              line : takeWhile isMoreType rest

    isMoreType :: BS.ByteString -> Bool
    isMoreType line =
        not (BS.null line)
        && Char.isSpace (BSC.head line)

    errorMessage =
        "Internal error in elm-repl function scrapeOutputType\n\
        \Please report this bug to <https://github.com/elm-lang/elm-repl/issues>"


freshLine :: BS.ByteString -> (BS.ByteString, BS.ByteString)
freshLine str
    | BS.null rest' = (line, "")
    | otherwise     = (line, BS.tail rest')
  where
    (line,rest') = BSC.break (=='\n') str

removeIfExists :: FilePath -> IO ()
removeIfExists fileName =
    do  exists <- doesFileExist fileName
        if exists
          then removeFile fileName
          else return ()
