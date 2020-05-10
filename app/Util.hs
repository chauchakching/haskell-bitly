module Util where

import qualified Data.Text                     as T
import           Web.Spock
import           Web.Spock.Config
import           Control.Monad.Logger           ( LoggingT
                                                , runStdoutLoggingT
                                                )
import           Database.Persist.Sqlite
import           Data.Aeson              hiding ( json )
import           Test.RandomStrings             ( randomASCII
                                                , onlyAlphaNum
                                                , randomString
                                                )

import           Type

runSQL
  :: (HasSpock m, SpockConn m ~ SqlBackend)
  => SqlPersistT (LoggingT IO) a
  -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn

errorJson :: Int -> T.Text -> ApiAction ()
errorJson code message = json $ object
  [ "result" .= String "failure"
  , "error" .= object ["code" .= code, "message" .= message]
  ]

randomCode :: IO T.Text
randomCode = T.pack <$> randomString (onlyAlphaNum randomASCII) 7
