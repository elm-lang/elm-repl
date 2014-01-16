module Action where

data Action = Command Command
            | Code Term
            | Skip
            deriving (Show, Eq)

data Command = AddFlag String
             | RemoveFlag String
             | ListFlags
             | ClearFlags
             | InfoFlags
             | Help
             | Exit
             | Reset
             deriving (Show, Eq)

type Term = (String, Maybe Def)

data Def = VarDef  String
         | DataDef String
         | Import  String
         deriving (Show, Eq)
