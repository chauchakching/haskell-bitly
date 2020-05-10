module Type where

import           Web.Spock

import           Database.Persist.Sqlite

data MySession = EmptySession
data MyAppState = EmptyAppState

type ApiAction a = SpockAction SqlBackend MySession MyAppState a
