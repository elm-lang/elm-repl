module Main where

import qualified Data.Char as Char
import qualified Data.List as List
import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@=?), assertFailure)
import qualified Test.HUnit.Base as HUnit
import Test.QuickCheck

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
     , testProperty "bad :commands don't work" badCommandNoParse
     ]
  , testGroup "Code parse tests"  [
     testCase "number parses"   $ codeParses "3"
     , testCase "type def parses before whitespac"   $ codeParses "type Foo = Bar"
     , testCase "data def parses after whitespace"   $ codeParses " data Baz = B { }"
     , testCase "number parses after newlines" $ codeParses "\n\n3"
     ]
  , testGroup "Whitespace parse tests" [
     testCase "empty is skipped" $ skipped ""
     , testCase "newlines are skipped" $ skipped "\n\n\n"
     , testProperty "skip all whitespace" $ skipAllSpace
     , testProperty "never skip non-whitespace" dontSkipNonSpace
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

badCommandNoParse :: Property
badCommandNoParse = forAll nonFlags noParseFlag
  where nonFlags = (arbitrary `suchThat` notFlag)
        noParseFlag s = either (const True) (const False) $ Parse.input (':':s)
        notFlag s = not . any (s `List.isPrefixOf`) $ ["help", "reset", "flags", "exit"]

skipAllSpace :: Property
skipAllSpace = forAll spaces $ either (const False) (==Action.Skip) . Parse.input
  where spaces = listOf . elements . filter Char.isSpace $ [toEnum 0..]

dontSkipNonSpace :: Property
dontSkipNonSpace = forAll notAllSpace $ either (const True) (/= Action.Skip) . Parse.input
  where notAllSpace = arbitrary `suchThat` (not . all Char.isSpace)
