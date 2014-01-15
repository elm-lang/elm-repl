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
     testCase ":help parses" $ actionParses (Action.Command Cmd.Help) ":help"
     ]
  ]

actionParses v s = case Parse.input s of
  Left err -> assertFailure err
  Right act -> v @=? act
