module Main where

import qualified Data.Char as Char
import qualified Data.List as List
import Data.Maybe (isJust)
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@=?))
import qualified Test.HUnit.Base as HUnit
import Test.QuickCheck

import qualified Input as I
import qualified Parse

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "Parse tests"
        [ testGroup "Command parse tests" cmdParseTests
        , testGroup "Code parse tests"  codeParseTests
        , testGroup "Whitespace parse tests" skipTests
        ]
    ]

cmdParseTests :: [Test]
cmdParseTests =
    [ testGroup "Good commands tests"
        [ testCase ":help parses"                    $ cmdParses (I.Help Nothing) ":help"
        , testCase ":reset parses after whitespace"  $ cmdParses I.Reset "  :reset"
        , testCase ":exit parses before whitespace"  $ cmdParses I.Exit ":exit   "
        ]
    , testGroup ":flags parse tests"
        [ testCase ":flags parses with Info"         $ cmdParses (I.InfoFlags Nothing) ":flags"
        , testCase ":flags + space parses with Info" $ cmdParses (I.InfoFlags Nothing) ":flags    "
        , testCase ":flags list parses"              $ cmdParses I.ListFlags ":flags list"
        , testCase ":flags clear parses w/ whitespace between" $
            cmdParses I.ClearFlags ":flags     clear"
        , testCase ":flags add source parses" $
            cmdParses (I.AddFlag "--src-dir=\"\"") ":flags add --src-dir=\"\""
        , testCase ":flags remove source parses" $
            cmdParses (I.RemoveFlag "--src-dir=bleh") ":flags remove --src-dir=bleh"
        ]
    , testGroup "Bad commands tests"
        [ testCase ":flagsgrbl triggers help" $ helpErr ":flagsg"
        , testProperty "bad :commands trigger help" badCommandHelp
        ]
    ]
  where
    cmdParses cmd = actionParses (I.Meta cmd)
    helpErr cmd =
        case Parse.rawInput cmd of
          I.Meta (I.Help message) ->
              HUnit.assert (isJust message)
          action ->
              HUnit.assertFailure (errorMessage action)

    errorMessage action =
        "Should display help with an error message, instead got: " ++ show action

codeParseTests :: [Test]
codeParseTests =
    [ testCase "number parses" $ codeParses Nothing "3"
    , testCase "number parses after newlines" $ codeParses Nothing "\n\n3"
    , testCase "data def parses"  $ codeParses (Just $ I.DataDef "Baz")  "data Baz = B { }"
    , testCase "var def parses" $ codeParses (Just $ I.VarDef "x") "x = 3"
    , testCase "var fun def parses" $ codeParses (Just $ I.VarDef "f") "f x = x"
    ]

skipTests :: [Test]
skipTests =
    [ testCase "empty is skipped" (skipped "")
    , testCase "newlines are skipped" (skipped "\n\n\n")
    , testProperty "skip all whitespace" skipAllSpace
    , testProperty "never skip non-whitespace" dontSkipNonSpace
    ]
  where
    skipped = actionParses I.Skip

-- | Test Helpers
codeParses :: Maybe I.DefName -> String -> HUnit.Assertion
codeParses name src =
    actionParses (I.Code (name, trimSpace src)) src
  where
    trimSpace = dropWhile Char.isSpace

actionParses :: I.Input -> String -> HUnit.Assertion
actionParses input rawString =
    input @=? Parse.rawInput rawString

badCommandHelp :: Property
badCommandHelp =
    forAll nonFlags helpParses
  where
    nonFlags =
        oneof
            [ arbitrary `suchThat` notFlag
            , badFlag
            ]

    helpParses s =
        case Parse.rawInput (':':s) of
          I.Meta (I.Help (Just _)) -> True
          _ -> False

    -- | TODO: things like help3
    notFlag s =
        not $ any (s `List.isPrefixOf`) flags

    badFlag =
      do  flag <- elements flags
          c <- arbitrary `suchThat` (not . Char.isSpace)
          return $ flag ++ [c]

    flags =
        [ "help", "reset", "flags", "exit" ]

skipAllSpace :: Property
skipAllSpace =
    forAll spaces $ (==I.Skip) . Parse.rawInput
  where
    spaces =
        listOf . elements $ filter Char.isSpace [toEnum 0..]

dontSkipNonSpace :: Property
dontSkipNonSpace =
    forAll notAllSpace $ (/= I.Skip) . Parse.rawInput
  where
    notAllSpace =
        arbitrary `suchThat` (not . all Char.isSpace)
