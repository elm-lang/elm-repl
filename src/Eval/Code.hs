{-# LANGUAGE OverloadedStrings #-}
module Eval.Code (eval) where

import Control.Monad.Cont (ContT(ContT, runContT))
import Control.Monad.Error (ErrorT, runErrorT, throwError)
import Control.Monad.RWS (get, modify)
import Control.Monad.Trans (liftIO)
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.List as List
import qualified Data.Map as Map
import System.Directory (doesFileExist, removeFile)
import System.FilePath ((</>), (<.>), replaceExtension)
import System.IO (hPutStrLn, stderr)

import qualified Environment as Env
import qualified Eval.Command as Eval
import qualified Input

import qualified Elm.Compiler.Module as Module
import qualified Elm.Compiler.Type as Type
import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Name as Name
import qualified Elm.Package.Paths as Path
import qualified Elm.Package.Version as Version
import qualified Elm.Utils as Utils


eval :: (Maybe Input.DefName, String) -> Eval.Command ()
eval code =
 do modify $ Env.insert code 
    env <- get
    liftIO $ writeFile tempElmPath (Env.toElmCode env)
    liftIO . runConts $ do
        runCmd (Env.compilerPath env) (Env.flags env ++ elmArgs)
        liftIO $ addNodeRunner tempJsPath
        value <- runCmd (Env.interpreterPath env) [tempJsPath]
        liftIO $ printIfNeeded value
        liftIO $ removeIfExists tempElmPath
        liftIO $ removeIfExists tempJsPath
    return ()
  where
    runConts m = runContT m (\_ -> return ())
    
    tempElmPath =
        "repl-temp-000" <.> "elm"

    tempJsPath =
        replaceExtension tempElmPath "js"
    
    elmArgs =
        [ tempElmPath
        , "--yes"
        , "--output=" ++ tempJsPath
        ]

printIfNeeded :: String -> IO ()
printIfNeeded rawValue =
  case rawValue of
    "" -> return ()
    _ ->
      do  tipe <- getType
          let value = init rawValue

          let isTooLong =
                List.isInfixOf "\n" value
                  || List.isInfixOf "\n" tipe
                  || length value + 3 + length tipe > 80

          let tipeAnnotation =
                if isTooLong
                  then "\n    : " ++ List.intercalate "\n      " (lines tipe)
                  else " : " ++ tipe

          putStrLn (value ++ tipeAnnotation)


runCmd :: FilePath -> [String] -> ContT () IO String
runCmd name args = ContT $ \ret ->
  do  result <- liftIO (Utils.unwrappedRun name args)
      case result of
        Right stdout ->
            ret stdout

        Left (Utils.MissingExe msg) ->
            liftIO $ hPutStrLn stderr msg

        Left (Utils.CommandFailed out err) ->
            liftIO $ hPutStrLn stderr (out ++ err)


addNodeRunner :: String -> IO ()
addNodeRunner tempJsPath =
    BS.appendFile tempJsPath nodeRunner


nodeRunner :: BS.ByteString
nodeRunner =
    BS.pack $
    concat
    [ "process.on('uncaughtException', function(err) {\n\
      \  process.stderr.write(err.toString());\n\
      \  process.exit(1);\n\
      \});\n\
      \var document = document || {};\n\
      \var window = window || {};\n\
      \var context = { inputs:[], addListener:function(){}, node:{} };\n\
      \var repl = Elm.Repl.make(context);\n\
      \var toString = Elm.Native.Show.make(context).toString;\n"
    , "if ('", Env.lastVarString, "' in repl) {\n"
    , "  console.log(toString(repl.", Env.lastVarString, "));\n"
    , "}"
    ]


getType :: IO String
getType =
  do  result <- runErrorT getTypeHelp
      case result of
        Right tipe -> return tipe
        Left _ -> return ""


getTypeHelp :: ErrorT String IO String
getTypeHelp =
  do  description <- Desc.read Path.description
      binary <- liftIO (BS.readFile (interfacePath description))
      let types = Module.interfaceTypes (Binary.decode binary)
      case Map.lookup Env.lastVarString types of
        Just tipe -> return (Type.toString tipe)
        Nothing -> throwError "Type signature not found!"


interfacePath :: Desc.Description -> FilePath
interfacePath description =
    Path.stuffDirectory
        </> "build-artifacts"
        </> Name.toFilePath (Desc.name description)
        </> Version.toString (Desc.version description)
        </> "Repl.elmi"


removeIfExists :: FilePath -> IO ()
removeIfExists fileName =
    do  exists <- doesFileExist fileName
        if exists
          then removeFile fileName
          else return ()
