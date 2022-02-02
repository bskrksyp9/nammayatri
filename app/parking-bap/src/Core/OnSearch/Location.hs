module Core.OnSearch.Location where

import Beckn.Prelude
import Core.Common.Gps (Gps)
import Core.OnSearch.Address (Address)

data Location = Location
  { id :: Text,
    gps :: Gps,
    address :: Address
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)