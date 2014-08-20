module Completion (complete)
       where

import Control.Monad.State (get)
import qualified Data.ByteString.Char8 as BS
import Data.Functor ((<$>))
import qualified Data.Trie as Trie
import Data.Trie (Trie)
import System.Console.Haskeline.Completion (Completion(Completion), CompletionFunc,
                                            completeQuotedWord, completeWord)

import Monad (ReplM)
import qualified Environment as Env

complete, completeIdentifier :: CompletionFunc ReplM
complete = completeQuotedWord Nothing "\"\'" (const $ return [] ) completeIdentifier
completeIdentifier = completeWord Nothing " \t" lookupCompletions

lookupCompletions :: String -> ReplM [Completion]
lookupCompletions s = completions s . removeReserveds . Env.defs <$> get
    where removeReserveds = Trie.delete Env.firstVar . Trie.delete Env.lastVar

completions :: String -> Trie a  -> [Completion]
completions s = Trie.lookupBy go (BS.pack s)
  where go :: Maybe a -> Trie a -> [Completion]
        go isElem suffixesTrie = maybeCurrent ++ suffixCompletions
          where maybeCurrent = case isElem of
                  Nothing -> []
                  Just _  -> [current]
                current = Completion s s True

                suffixCompletions = map (suffixCompletion . BS.unpack) . Trie.keys $ suffixesTrie
                suffixCompletion suf = Completion full full False
                  where full = s ++ suf
