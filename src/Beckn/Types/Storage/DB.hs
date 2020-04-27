{-# LANGUAGE DeriveAnyClass #-}

module Beckn.Types.Storage.DB where

import           EulerHS.Prelude          hiding (id)

import qualified Beckn.Types.Storage.User as User
import qualified Database.Beam            as B



data BecknDb f =
  BecknDb
    {
      _user  :: f (B.TableEntity User.UserT)
     }
  deriving (Generic, B.Database be)

becknDb :: B.DatabaseSettings be BecknDb
becknDb =
  B.defaultDbSettings `B.withDbModification`
      B.dbModification
        {
        _user = User.fieldEMod
        }
