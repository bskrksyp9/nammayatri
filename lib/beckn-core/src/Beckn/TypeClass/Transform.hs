{-# LANGUAGE MultiParamTypeClasses #-}

module Beckn.TypeClass.Transform where

import qualified EulerHS.Language                        as L

class Transform f g  | f -> g, g ->f where
  transform :: f -> g -> g
  transformFlow :: f -> L.Flow g