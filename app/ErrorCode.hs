module ErrorCode where

import           Web.Spock
import           Network.HTTP.Types.Status
import           Util

invalidUrlErrorJson = do
  setStatus status400
  errorJson 1 "Error: Invalid url"

unknownUrlCodeErrorJson = do
  setStatus status404
  errorJson 2 "Error: Unknown url code"
