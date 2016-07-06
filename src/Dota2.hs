
module Dota2
    ( getLeagueListing
    ) where

import           Data.Proxy          (Proxy (..))
import           Network.HTTP.Client (Manager)
import           Servant.API         ((:<|>) (..))
import           Servant.Client      (BaseUrl (..), ClientM, Scheme (Http),
                                      client)

import           Dota2.API
import           Dota2.Types

dota2API :: Proxy Dota2API
dota2API = Proxy

-- | Get league listing.
-- (<https://wiki.teamfortress.com/wiki/WebAPI/GetLeagueListing see>)
getLeagueListing
    :: Maybe SteamKey
    -> Maybe Language
    -> Manager
    -> BaseUrl
    -> ClientM (Result Leagues)

-- | Get live league games
-- (<https://wiki.teamfortress.com/wiki/WebAPI/GetLiveLeagueGames see>
getLiveLeagueGames :: Maybe SteamKey
                   -> Manager
                   -> BaseUrl
                   -> ClientM (Result Games)

-- | Get match details by given match id
-- (<https://wiki.teamfortress.com/wiki/WebAPI/GetMatchDetails#Tower_Status see>)
getMatchDetails :: Maybe SteamKey
                -> Maybe MatchId
                -> Manager
                -> BaseUrl
                -> ClientM (Result Match)

(getLeagueListing :<|> getLiveLeagueGames :<|> getMatchDetails) =
    client dota2API