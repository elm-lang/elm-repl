module Main where

import qualified Data.Char as Char
import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import qualified Test.HUnit.Base as HUnit
import Test.HUnit ((@=?), assertFailure)

import qualified Action
import qualified Command as Cmd
import qualified Parse

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
  testGroup "Command parse tests" [
     testCase ":help parses"  $ cmdParses Cmd.Help ":help"
     , testCase ":reset parses after whitespace"  $ cmdParses Cmd.Reset "  :reset"
     , testCase ":exit parses before whitespace"  $ cmdParses Cmd.Exit ":exit   "
     , testGroup ":flags parse tests" [
       testCase ":flags parses with Info" $ cmdParses Cmd.InfoFlags ":flags"
       , testCase ":flags list parses" $ cmdParses Cmd.ListFlags ":flags list"
       , testCase ":flags clear parses w/ whitespace between" $
         cmdParses Cmd.ClearFlags ":flags     clear"
       , testCase ":flags add source parses" $
         cmdParses (Cmd.AddFlag "--src-dir=\"\"") ":flags add --src-dir=\"\""
       , testCase ":flags remove source parses" $
         cmdParses (Cmd.RemoveFlag "--src-dir=bleh") ":flags remove --src-dir=bleh"
       ]
     ]
  , testGroup "Code parse tests"  [
     testCase "number parses"   $ codeParses "3"
     , testCase "type def parses before whitespac"   $ codeParses "type Foo = Bar"
     , testCase "data def parses after whitespace"   $ codeParses " data Baz = B { }"
     , testCase "number parses after newlines" $ codeParses "\n\n3"
     ]
  , testGroup "Empty parse tests" [
     testCase "empty is skipped" $ skipped ""
     , testCase "newlines are skipped" $ skipped "\n\n\n"
     ]
  ]
  where codeParses src = actionParses (Action.Code . trimSpace $ src) src
        cmdParses cmd src = actionParses (Action.Command cmd) src
        skipped src = actionParses Action.Skip src

        trimSpace = dropWhile Char.isSpace

actionParses :: Action.Action -> String -> HUnit.Assertion
actionParses v s = case Parse.input s of
  Left err -> assertFailure err
  Right act -> v @=? act
