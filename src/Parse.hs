module Parse where

import Data.Functor ((<$), (<$>))
import Text.Parsec

import qualified Data.Char as Char
import qualified Data.List as List

import Action

type Parser = Parsec String ()

input :: String -> Either String Action
input = either (Left . show) Right . parse result ""

result :: Parser Action
result = do
  spaces
  (Skip <$ eof)
    <|> do
    c <- lookAhead anyChar
    if c == ':'
      then char ':' >> Command <$> command
      else Code . mkTerm <$> many anyChar

command :: Parser Command
command = choice (flags : basics)
    where
      basics = map basicCommand [ ("exit",Exit), ("reset",Reset), ("help",Help) ]

      basicCommand (name,command) =
          string name >> spaces >> eof >> return command

flags :: Parser Command
flags = do
  string "flags"
  many space
  choice [ srcDirFlag "add"    AddFlag
         , srcDirFlag "remove" RemoveFlag
         , ListFlags  <$ string "list"
         , ClearFlags <$ string "clear"
         , return InfoFlags
         ]
    where srcDirFlag name ctor = do
            string name
            many1 space
            ctor <$> srcDir

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
        getFirstCap (token@(c:_):rest) = if Char.isUpper c
                                           then token
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
        _ -> error "Parse.hs: Case error. Please submit bug report to https://github.com/evancz/elm-repl/issues."
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
