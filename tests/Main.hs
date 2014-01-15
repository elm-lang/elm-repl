module Main where

import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import qualified Test.HUnit.Base as HUnit
import Test.HUnit ((@=?), assertFailure)
import Text.Parsec

import qualified Action
import qualified Command as Cmd
import qualified Parse

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
  testGroup "Command parse tests" [
       testCase ":help parses"  $ cmdParses Cmd.Help ":help "
  ]
  , testGroup "Code parse tests"  [
     testCase "number parses"   $ codeParses "3"
     ]
  ]
  where codeParses src = actionParses (Action.Code src) src
        cmdParses cmd src = actionParses (Action.Command cmd) src

actionParses v s = case Parse.input s of
  Left err -> assertFailure err
  Right act -> v @=? act
