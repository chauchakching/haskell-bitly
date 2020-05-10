module Main where

import           Web.Spock
import           Web.Spock.Config
import           Data.Aeson              hiding ( json )
import           GHC.Generics

-- import           Control.Monad.Trans
-- import           Data.IORef
import           Control.Monad.IO.Class         ( liftIO )
import qualified Data.Text                     as T
import           Control.Monad.Logger           ( runStdoutLoggingT )
import           Database.Persist        hiding ( get )
import qualified Database.Persist              as P
import           Database.Persist.Sqlite hiding ( get )
import           Database.Persist.TH
import           Type
import           Util
import           ErrorCode

-- TODO:
-- 

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Link json
    url T.Text
    code T.Text
    Primary code
    deriving Show
|]

data TheLink = TheLink {
  theLinkUrl :: T.Text
} deriving (Generic, Show)
instance FromJSON TheLink

main :: IO ()
main = do
  pool     <- runStdoutLoggingT $ createSqlitePool "api.db" 5

  spockCfg <- defaultSpockCfg EmptySession (PCPool pool) EmptyAppState

  runStdoutLoggingT $ runSqlPool
    (do
      runMigration migrateAll
    )
    pool
  runSpock 3000 (spock spockCfg app)

app :: SpockM SqlBackend MySession MyAppState ()
app = do
  get ("api" <//> "bitly" <//> var) $ \code -> do
    maybeLink <- runSQL $ selectFirst [LinkCode ==. code] []
    case maybeLink of
      Nothing   -> invalidUrlErrorJson
      Just link -> json $ object ["url" .= (linkUrl $ entityVal link)]

  post ("api" <//> "bitly") $ do
    theLink  <- jsonBody' :: ApiAction TheLink

    linkCode <- liftIO randomCode
    newId    <- runSQL $ insert Link { linkUrl = theLinkUrl theLink, linkCode }

    json $ object ["code" .= linkCode]

  get var $ \code -> do
    maybeLink <- runSQL $ selectFirst [LinkCode ==. code] []
    case maybeLink of
      Nothing   -> unknownUrlCodeErrorJson
      Just link -> redirect $ linkUrl $ entityVal link
