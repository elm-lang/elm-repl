module Action where

import qualified Command as Cmd

data Action = Command Cmd.Command
            | Code String
            | Skip
            deriving (Show, Eq)

