{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Dota2.API
    ( Dota2API
    ) where

import           Servant.API ((:>), Get, JSON, QueryParam)

import           Dota2.Types

type Dota2API = "IDOTA2Match_570" :>
              ( "GetLeagueListing" :> "v1" :> QueryParam "key" SteamKey :> QueryParam "language" Language :> Get '[JSON] (Result Leagues)
              )
