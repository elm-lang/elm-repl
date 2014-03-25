module Parse where

import Data.Functor ((<$), (<$>))
import Text.Parsec

import qualified Data.Char as Char
import qualified Data.List as List

import Action

type Parser = Parsec String ()

input :: String -> Action
input inp = case parse result "" inp of
  Left err  -> Command . Help . Just . show $ err
  Right act -> act

result :: Parser Action
result = do
  spaces
  skip <|> cmd <|> term
  where
    skip = Skip <$ eof
    cmd  = char ':' >> Command <$> command
    term = Code . mkTerm <$> many anyChar

command :: Parser Command
command = do
  flag <- many1 notSpace
  spaces
  case flag of
    "exit"  -> basicCommand Exit
    "reset" -> basicCommand Reset
    "help"  -> basicCommand $ Help Nothing
    "flags" -> basicCommand (InfoFlags Nothing) <|> flags
    _       -> return $ Help . Just $ flag
  where basicCommand cmd = cmd <$ eof

flags :: Parser Command
flags = do
  flag <- many1 notSpace
  case flag of
    "add"    -> srcDirFlag AddFlag
    "remove" -> srcDirFlag RemoveFlag
    "list"   -> return ListFlags
    "clear"  -> return ClearFlags
    _        -> return $ InfoFlags . Just $ flag
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

mkTerm :: String -> Term
mkTerm src = (src, mkCode src)

mkCode :: String -> Maybe Def
mkCode src
  | List.isPrefixOf "import " src = 
    let name = getFirstCap . words $ src
        getFirstCap (tok@(c:_):rest) = if Char.isUpper c
                                       then tok
                                       else getFirstCap rest
        getFirstCap _ = src
    in Just $ Import name

  | List.isPrefixOf "data " src =
      let name = takeWhile (/=' ') . drop 5 $ src
      in  Just $ DataDef name

  | otherwise = case break (=='=') src of
        (_,"") -> Nothing
        (beforeEquals, _:c:_)
          | Char.isSymbol c || hasLet beforeEquals || hasBrace beforeEquals ->
            Nothing
          | otherwise -> Just . VarDef $ declName beforeEquals
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
