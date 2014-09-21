module Parse (inputToAction) where

import qualified Data.Char as Char
import Data.Functor ((<$>))
import qualified Data.List as List
import Text.Parsec (Parsec, (<|>), anyChar, char, choice, eof, many, many1,
                    manyTill, parse, satisfy, space, spaces, string)

import Action (Action, Command)
import qualified Action as A

type Parser = Parsec String ()

inputToAction :: String -> Action
inputToAction input =
    case parse result "" input of
      Right action -> action
      Left errorMessage ->
          A.Command . A.Help $ Just (show errorMessage)

result :: Parser Action
result =
  do  spaces
      skip <|> cmd <|> term
  where
    skip = eof >> return A.Skip
    cmd  = char ':' >> A.Command <$> command
    term = A.Code . mkTerm <$> many anyChar

command :: Parser Command
command =
  do  flag <- many1 notSpace
      spaces
      case flag of
        "exit"  -> basicCommand A.Exit
        "reset" -> basicCommand A.Reset
        "help"  -> basicCommand (A.Help Nothing)
        "flags" -> basicCommand (A.InfoFlags Nothing) <|> flags
        _       -> return $ A.Help (Just flag)
  where
    basicCommand cmd =
        eof >> return cmd


flags :: Parser Command
flags =
  do  flag <- many1 notSpace
      case flag of
        "add"    -> srcDirFlag A.AddFlag
        "remove" -> srcDirFlag A.RemoveFlag
        "list"   -> return A.ListFlags
        "clear"  -> return A.ClearFlags
        _        -> return $ A.InfoFlags . Just $ flag
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


mkTerm :: String -> A.Term
mkTerm src = (src, mkCode src)


mkCode :: String -> Maybe A.Def
mkCode src
    | List.isPrefixOf "import " src =
        let getFirstCap tokens =
                case tokens of
                  token@(c:_) : rest ->
                      if Char.isUpper c then token else getFirstCap rest
                  _ -> src
        in
            Just $ A.Import (getFirstCap (words src))

    | List.isPrefixOf "data " src =
        let name = takeWhile (/=' ') . drop 5 $ src
        in  Just $ A.DataDef name

    | otherwise =
        case break (=='=') src of
          (_,"") -> Nothing

          (beforeEquals, _:c:_) ->
              if Char.isSymbol c || hasLet beforeEquals || hasBrace beforeEquals
                  then Nothing
                  else Just $ A.VarDef (declName beforeEquals)

          _ -> error errorMessage

        where
          errorMessage =
              "Internal error in elm-repl function Parse.mkCode\n\
              \Please submit bug report to <https://github.com/elm-lang/elm-repl/issues>"

          declName pattern =
              case takeWhile Char.isSymbol $ dropWhile (not . Char.isSymbol) pattern of
                "" -> takeWhile (/=' ') pattern
                op -> op


hasLet :: String -> Bool
hasLet body =
    elem "let" $ map token (words body)
  where
    isVarChar c =
        Char.isAlpha c || Char.isDigit c || elem c "_'"

    token word =
        takeWhile isVarChar $ dropWhile (not . Char.isAlpha) word


hasBrace :: String -> Bool
hasBrace body =
    elem '{' body
