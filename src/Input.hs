module Input where

data Input
    = Meta Command
    | Code (Maybe DefName, String)
    | Skip
    deriving (Show, Eq)

data Command
    = AddFlag String
    | RemoveFlag String
    | ListFlags
    | ClearFlags
      -- Just if this was triggered by an error
    | InfoFlags (Maybe String)
    | Help (Maybe String)
    | Exit
    | Reset
    deriving (Show, Eq)

data DefName
    = VarDef  String
    | DataDef String
    | Import  String
    deriving (Show, Eq)
