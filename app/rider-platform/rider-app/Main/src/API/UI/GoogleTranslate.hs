module API.UI.GoogleTranslate
  ( API,
    handler,
  )
where

import qualified Domain.Types.Person as Person
import Environment (FlowHandler, FlowServer)
import EulerHS.Prelude
import qualified Kernel.External.GoogleTranslate.Types as GoogleTranslate
import Kernel.Types.App
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI, withPersonIdLogTag)
import Servant
import qualified SharedLogic.GoogleTranslate as GoogleTranslate
import Tools.Auth
import qualified Tools.Maps as Maps

type API =
  "language"
    :> ( "translate"
           :> TokenAuth
           :> MandatoryQueryParam "source" Maps.Language
           :> MandatoryQueryParam "target" Maps.Language
           :> MandatoryQueryParam "q" Text
           :> Get '[JSON] GoogleTranslate.TranslateResp
       )

handler :: FlowServer API
handler =
  translate

translate :: Id Person.Person -> Maps.Language -> Maps.Language -> Text -> FlowHandler GoogleTranslate.TranslateResp
translate personId source target = withFlowHandlerAPI . withPersonIdLogTag personId . GoogleTranslate.translate source target