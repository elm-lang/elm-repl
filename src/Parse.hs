module Parse where

import qualified Data.Char as Char
import Data.Functor ((<$), (<$>))
import qualified Data.List as List
import Text.Parsec (Parsec, (<|>), anyChar, char, choice, eof, many, many1,
                    manyTill, parse, satisfy, space, spaces, string)

import Action (Action, Command)
import qualified Action as A

type Parser = Parsec String ()

input :: String -> Action
input inp = case parse result "" inp of
  Left err  -> A.Command . A.Help . Just . show $ err
  Right act -> act

result :: Parser Action
result = do
  spaces
  skip <|> cmd <|> term
  where
    skip = A.Skip <$ eof
    cmd  = char ':' >> A.Command <$> command
    term = A.Code . mkTerm <$> many anyChar

command :: Parser Command
command = do
  flag <- many1 notSpace
  spaces
  case flag of
    "exit"  -> basicCommand A.Exit
    "reset" -> basicCommand A.Reset
    "help"  -> basicCommand $ A.Help Nothing
    "flags" -> basicCommand (A.InfoFlags Nothing) <|> flags
    _       -> return $ A.Help . Just $ flag
  where basicCommand cmd = cmd <$ eof

flags :: Parser Command
flags = do
  flag <- many1 notSpace
  case flag of
    "add"    -> srcDirFlag A.AddFlag
    "remove" -> srcDirFlag A.RemoveFlag
    "list"   -> return A.ListFlags
    "clear"  -> return A.ClearFlags
    _        -> return $ A.InfoFlags . Just $ flag
  where srcDirFlag ctor = do
          many1 space
          ctor <$> srcDir

notSpace :: Parser Char
notSpace = satisfy $ not . Char.isSpace

srcDir :: Parser String
srcDir = do
  string "--src-dir="
  dir <- manyTill anyChar (choice [ space >> return (), eof ])
  return $ "--src-dir=" ++ dir

mkTerm :: String -> A.Term
mkTerm src = (src, mkCode src)

mkCode :: String -> Maybe A.Def
mkCode src
  | List.isPrefixOf "import " src = 
    let name = getFirstCap . words $ src
        getFirstCap (tok@(c:_):rest) = if Char.isUpper c
                                       then tok
                                       else getFirstCap rest
        getFirstCap _ = src
    in Just $ A.Import name

  | List.isPrefixOf "data " src =
      let name = takeWhile (/=' ') . drop 5 $ src
      in  Just $ A.DataDef name

  | otherwise = case break (=='=') src of
        (_,"") -> Nothing
        (beforeEquals, _:c:_)
          | Char.isSymbol c || hasLet beforeEquals || hasBrace beforeEquals ->
            Nothing
          | otherwise -> Just . A.VarDef $ declName beforeEquals
        _ -> error "Parse.hs: Case error. Please submit bug report to https://github.com/elm-lang/elm-repl/issues."
        where
          declName pattern =
              case takeWhile Char.isSymbol . dropWhile (not . Char.isSymbol) $ pattern of
                "" -> takeWhile (/=' ') pattern
                op -> op

          hasLet = elem "let" . map token . words
            where
              isVarChar c = Char.isAlpha c || Char.isDigit c || elem c "_'"
              token = takeWhile isVarChar . dropWhile (not . Char.isAlpha)

          hasBrace = elem '{'
