module Beckn.External.MyValuesFirst.API where

import Beckn.External.MyValuesFirst.Types
import Beckn.Types.App
  ( MandatoryQueryParam,
  )
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Servant
import Servant.API.ContentTypes
import Servant.Client

type ServiceAPI =
  "smpp"
    :> "sendsms"
    :> MandatoryQueryParam "username" Text
    :> MandatoryQueryParam "password" Text
    :> MandatoryQueryParam "from" Text
    :> MandatoryQueryParam "to" Text
    :> MandatoryQueryParam "text" Text
    :> Post '[PlainText] NoContent

serviceAPI :: Proxy ServiceAPI
serviceAPI = Proxy

submitSms SubmitSms {..} =
  void $ ET.client serviceAPI _username _password _from _to _text
