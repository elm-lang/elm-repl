module Parse where

import Data.Functor ((<$), (<$>))
import Text.Parsec

import Action
import Command as Cmd

type Parser = Parsec String ()

input :: String -> Either String Action
input = either (Left . show) Right . parse result ""

result :: Parser Action
result = do
  spaces
  (Skip <$ eof)
    <|> do
    c <- anyChar
    if c == ':'
      then Command <$> command
      else Code . (c:) <$> many anyChar

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
