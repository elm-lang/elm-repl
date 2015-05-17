module Read (input) where

import qualified Data.Char as Char
import Data.Functor ((<$>))
import qualified Data.List as List
import Text.Parsec
  ( Parsec, (<|>), anyChar, char, choice, eof, many, many1
  , manyTill, parse, satisfy, space, spaces, string
  )

import qualified Environment as Env


type Parser = Parsec String ()


input :: String -> Env.Input
input string =
  case parse result "" string of
    Right action ->
        action

    Left errorMessage ->
        Env.Meta (Env.Help (Just (show errorMessage)))


result :: Parser Env.Input
result =
  do  spaces
      choice
        [ do  eof
              return Env.Skip
        , do  char ':'
              Env.Meta <$> config
        , do  string <- many anyChar
              return (Env.Code (extractCode string))
        ]


-- PARSE CONFIG

config :: Parser Env.Config
config =
  let
    ok cmd =
        eof >> return cmd
  in
  do  flag <- many1 notSpace
      spaces
      case flag of
        "exit"  -> ok Env.Exit
        "reset" -> ok Env.Reset
        "help"  -> ok (Env.Help Nothing)
        "flags" -> ok (Env.InfoFlags Nothing) <|> flags
        _       -> return $ Env.Help (Just flag)


flags :: Parser Env.Config
flags =
  do  flag <- many1 notSpace
      case flag of
        "add"    -> srcDirFlag Env.AddFlag
        "remove" -> srcDirFlag Env.RemoveFlag
        "list"   -> return Env.ListFlags
        "clear"  -> return Env.ClearFlags
        _        -> return (Env.InfoFlags (Just flag))
  where
    srcDirFlag ctor =
      do  many1 space
          ctor <$> srcDir


notSpace :: Parser Char
notSpace =
    satisfy (not . Char.isSpace)


srcDir :: Parser String
srcDir =
  do  string "--src-dir="
      dir <- manyTill anyChar (choice [ space >> return (), eof ])
      return ("--src-dir=" ++ dir)


-- PARSE CODE

extractCode :: String -> (Maybe Env.DefName, String)
extractCode rawInput =
    (extractDefName rawInput, rawInput)


extractDefName :: String -> Maybe Env.DefName
extractDefName src
  | List.isPrefixOf "import " src =
      let
        getFirstCap tokens =
            case tokens of
              token@(c:_) : rest ->
                  if Char.isUpper c then token else getFirstCap rest
              _ -> src
      in
        Just (Env.Import (getFirstCap (words src)))

  | List.isPrefixOf "type alias " src =
      let
        name = takeWhile (/=' ') (drop 11 src)
      in
        Just (Env.DataDef name)

  | List.isPrefixOf "type " src =
      let
        name = takeWhile (/=' ') (drop 5 src)
      in
        Just (Env.DataDef name)

  | otherwise =
      case break (=='=') src of
        (_,"") ->
            Nothing

        (beforeEquals, _:c:_) ->
            if Char.isSymbol c || hasLet beforeEquals || hasBrace beforeEquals
              then Nothing
              else Just (Env.VarDef (declName beforeEquals))

        _ ->
            Nothing

      where
        declName pattern =
            case takeWhile Char.isSymbol $ dropWhile (not . Char.isSymbol) pattern of
              "" -> takeWhile (/=' ') pattern
              op -> op


hasLet :: String -> Bool
hasLet body =
    elem "let" (map token (words body))
  where
    isVarChar c =
        Char.isAlpha c || Char.isDigit c || elem c "_'"

    token word =
        takeWhile isVarChar $ dropWhile (not . Char.isAlpha) word


hasBrace :: String -> Bool
hasBrace body =
    elem '{' body
