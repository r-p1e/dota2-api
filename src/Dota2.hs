
module Dota2
    ( getLeagueListing
    ) where

import           Data.Proxy          (Proxy (..))
import           Network.HTTP.Client (Manager)
import           Servant.Client      (BaseUrl (..), ClientM, Scheme (Http),
                                      client)

import           Dota2.API
import           Dota2.Types

dota2API :: Proxy Dota2API
dota2API = Proxy

getLeagueListing
    :: Maybe SteamKey
    -> Maybe Language
    -> Manager
    -> BaseUrl
    -> ClientM (Result Leagues)

getLeagueListing = client dota2API
