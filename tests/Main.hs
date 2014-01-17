module Main where

import qualified Data.Char as Char
import qualified Data.List as List
import Data.Maybe (isJust)
import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@=?))
import qualified Test.HUnit.Base as HUnit
import Test.QuickCheck

import Action
import qualified Parse

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ testGroup "Parse tests" [
             testGroup "Command parse tests" cmdParseTests
             , testGroup "Code parse tests"  codeParseTests
             , testGroup "Whitespace parse tests" skipTests
             ]
        ]

cmdParseTests = [
  testGroup "Good commands tests" [
     testCase ":help parses"                      $ cmdParses (Help Nothing) ":help"
     , testCase ":reset parses after whitespace"  $ cmdParses Reset "  :reset"
     , testCase ":exit parses before whitespace"  $ cmdParses Exit ":exit   "
     ]
  , testGroup ":flags parse tests" [
     testCase ":flags parses with Info"           $ cmdParses (InfoFlags Nothing) ":flags"
     , testCase ":flags + space parses with Info" $ cmdParses (InfoFlags Nothing) ":flags    "
     , testCase ":flags list parses"              $ cmdParses ListFlags ":flags list"
     , testCase ":flags clear parses w/ whitespace between" $
         cmdParses ClearFlags ":flags     clear"
     , testCase ":flags add source parses" $
         cmdParses (AddFlag "--src-dir=\"\"") ":flags add --src-dir=\"\""
     , testCase ":flags remove source parses" $
         cmdParses (RemoveFlag "--src-dir=bleh") ":flags remove --src-dir=bleh"
     ]

  , testGroup "Bad commands tests" [
     testCase ":flagsgrbl triggers help" $ helpErr ":flagsg"
     , testProperty "bad :commands trigger help" badCommandHelp
     ]
  ]
  where cmdParses cmd = actionParses (Action.Command cmd)
        helpErr   cmd = case Parse.input cmd of
          Action.Command (Help m) -> HUnit.assert $ isJust m
          res                     ->
            HUnit.assertFailure
            ("Should display help with an error message, instead got: "
             ++ show res)

codeParseTests = [
  testCase "number parses" $ codeParses Nothing "3"
  , testCase "number parses after newlines" $ codeParses Nothing "\n\n3"
  , testCase "data def parses"  $ codeParses (Just $ DataDef "Baz")  "data Baz = B { }"
  , testCase "var def parses" $ codeParses (Just $ VarDef "x") "x = 3"
  , testCase "var fun def parses" $ codeParses (Just $ VarDef "f") "f x = x"
  ]

skipTests = [
  testCase "empty is skipped"       $ skipped ""
  , testCase "newlines are skipped" $ skipped "\n\n\n"
  , testProperty "skip all whitespace"       skipAllSpace
  , testProperty "never skip non-whitespace" dontSkipNonSpace
  ]
  where skipped = actionParses Action.Skip

-- | Test Helpers
codeParses :: Maybe Def -> String -> HUnit.Assertion
codeParses code src = actionParses (Action.Code (trimSpace src, code)) src
  where trimSpace = dropWhile Char.isSpace

actionParses :: Action.Action -> String -> HUnit.Assertion
actionParses v s = v @=? Parse.input s

badCommandHelp :: Property
badCommandHelp = forAll nonFlags helpParses
  where nonFlags = oneof [ arbitrary `suchThat` notFlag
                         , badFlag
                         ]
        helpParses s = case Parse.input (':':s) of
          Command (Help (Just _)) -> True
          _             -> False
        -- | TODO: things like help3
        notFlag s = not . any (s `List.isPrefixOf`) $ flags
        badFlag = do
          flag <- elements flags
          c <- arbitrary `suchThat` (not . Char.isSpace)
          return $ flag ++ [c]
        flags = ["help", "reset", "flags", "exit"]

skipAllSpace :: Property
skipAllSpace = forAll spaces $ (==Action.Skip) . Parse.input
  where spaces = listOf . elements . filter Char.isSpace $ [toEnum 0..]

dontSkipNonSpace :: Property
dontSkipNonSpace = forAll notAllSpace $ (/= Action.Skip) . Parse.input
  where notAllSpace = arbitrary `suchThat` (not . all Char.isSpace)
