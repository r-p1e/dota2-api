{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DeriveGeneric  #-}

module Dota2.Types where

import           Data.Aeson       (FromJSON (..), genericParseJSON)
import           Data.Aeson.Types (Options (..), Value (Number), camelTo2,
                                   defaultOptions, typeMismatch)
import           Data.Bits        (Bits ((.&.)))
import           Data.Text        (Text)
import           GHC.Generics     (Generic)
import           Web.HttpApiData  (ToHttpApiData (..))

import qualified Data.Scientific  as Scientific

-- | Your Steam Web API key. Without this, the server will return an HTTP 403
--  (forbidden) error. A key can be generated <http://steamcommunity.com/dev/apikey here>.
newtype SteamKey =
    SteamKey Text
    deriving (Generic)

instance ToHttpApiData SteamKey where
  toQueryParam (SteamKey sk) = sk

-- | The <https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes ISO639-1 language code>
-- for the language all tokenized strings should be returned in. Not all strings
-- have been translated to every language. If a language does not have a string,
-- the English string will be returned instead. If this parameter is omitted the
-- string token will be returned for the strings.
newtype Language = Language Text deriving Generic

instance ToHttpApiData Language where
  toQueryParam (Language lang) = lang


data Leagues = Leagues
    { leagues :: [League]  -- A list of leagues
    } deriving (Generic)

instance FromJSON Leagues

data League = League
    { lName          :: Text  -- The name of the league
    , lLeagueId      :: Text  -- The league's unique ID
    , lDescription   :: Text  -- A description of the league
    , lTournamentUrl :: Text  -- The league's website
    } deriving (Generic)

instance FromJSON League where
    parseJSON =
        genericParseJSON
            defaultOptions
            { fieldLabelModifier = camelTo2 '_' . drop 1
            }

data Games = Games
    { games :: [Game]  -- A list of games
    } deriving (Generic)

instance FromJSON Games

data Game = Game
    { gPlayers     :: [Player]  -- The list of players within a game
    , gRadiantTeam :: Team  -- Information about the team playing as radiant for this match
    , gDireTeam    :: Team  -- Information about the team playing as dire for this match
    , gLobbyId     :: Text  -- Unique ID for the matches lobby.
    , gSpectators  :: Int  -- Number of spectators at time of query
    , gTowerState  :: TowerState  -- See 'TowerState' below
    , gLeagueId    :: Text  -- Unique ID for the league of the match being played. A list of league IDs can be found via the 'getLeagueListing' method.
    } deriving (Generic)

instance FromJSON Game where
    parseJSON =
        genericParseJSON
            defaultOptions
            { fieldLabelModifier = camelTo2 '_' . drop 1
            }

data Player = Player
    { pAccountId :: Int  -- 32-bit account ID
    , pName      :: Text  -- the player's display name
    , pHeroId    :: Int  -- The hero's unique ID. A lis of hero IDs can be found via the 'getHeroes' method
    , pTeam      :: TeamType  -- What team the player is current playing as
    } deriving (Generic)

instance FromJSON Player where
    parseJSON =
        genericParseJSON
            defaultOptions
            { fieldLabelModifier = camelTo2 '_' . drop 1
            }

-- | What team the player is current playing as
data TeamType
    = Radiant
    | Dire
    | Broadcaster
    | Unassigned

instance FromJSON TeamType where
    parseJSON v@(Number n) =
        case n of
            0 -> return Radiant
            1 -> return Dire
            2 -> return Broadcaster
            4 -> return Unassigned
            _ -> typeMismatch "TypeMismatch. Wrong bit mask" v
    parseJSON invalid = typeMismatch "TeamType" invalid


data Team = Team
    { tTeamName :: Text  -- The team's name
    , tTeamId   :: Text  -- The team's unique ID.
    , tTeamLogo :: Text  -- The UGC id for the team logo. You can resolve this with the 'getUGCFileDetails' method
    , tComplete :: Bool  -- Whether the players for this team are all team members
    } deriving (Generic)

instance FromJSON Team where
    parseJSON =
        genericParseJSON
            defaultOptions
            { fieldLabelModifier = camelTo2 '_' . drop 1
            }

-- | The matches tower status
data TowerState = TowerState
    { direAncientTop       :: Bool
    , direAncientBottom    :: Bool
    , direBottomTier3      :: Bool
    , direBottomTier2      :: Bool
    , direBottomTier1      :: Bool
    , direMiddleTier3      :: Bool
    , direMiddleTier2      :: Bool
    , direMiddleTier1      :: Bool
    , direTopTier3         :: Bool
    , direTopTier2         :: Bool
    , direTopTier1         :: Bool
    , radiantAncientTop    :: Bool
    , radiantAncientBottom :: Bool
    , radiantBottomTier3   :: Bool
    , radiantBottomTier2   :: Bool
    , radiantBottomTier1   :: Bool
    , radiantMiddleTier3   :: Bool
    , radiantMiddleTier2   :: Bool
    , radiantMiddleTier1   :: Bool
    , radiantTopTier3      :: Bool
    , radiantTopTier2      :: Bool
    , radiantTopTier1      :: Bool
    }

instance FromJSON TowerState where
    parseJSON v@(Number n) =
        case Scientific.floatingOrInteger n of
            Right i ->
                return
                    TowerState
                    { radiantTopTier1 = checkBit i 0b1
                    , radiantTopTier2 = checkBit i 0b10
                    , radiantTopTier3 = checkBit i 0b100
                    , radiantMiddleTier1 = checkBit i 0b1000
                    , radiantMiddleTier2 = checkBit i 0b10000
                    , radiantMiddleTier3 = checkBit i 0b100000
                    , radiantBottomTier1 = checkBit i 0b1000000
                    , radiantBottomTier2 = checkBit i 0b10000000
                    , radiantBottomTier3 = checkBit i 0b100000000
                    , radiantAncientBottom = checkBit i 0b1000000000
                    , radiantAncientTop = checkBit i 0b10000000000
                    , direTopTier1 = checkBit i 0b100000000000
                    , direTopTier2 = checkBit i 0b1000000000000
                    , direTopTier3 = checkBit i 0b10000000000000
                    , direMiddleTier1 = checkBit i 0b100000000000000
                    , direMiddleTier2 = checkBit i 0b1000000000000000
                    , direMiddleTier3 = checkBit i 0b10000000000000000
                    , direBottomTier1 = checkBit i 0b100000000000000000
                    , direBottomTier2 = checkBit i 0b1000000000000000000
                    , direBottomTier3 = checkBit i 0b10000000000000000000
                    , direAncientBottom = checkBit i 0b100000000000000000000
                    , direAncientTop = checkBit i 0b1000000000000000000000
                    }
            Left r ->
                typeMismatch
                    "TowerState must be 32-bit unsigned integer not floating point"
                    v
    parseJSON invalid = typeMismatch "failed when parse TowerState" invalid

-- | Common API response datatype
data Result a = Result
    { result :: a
    } deriving Generic

instance FromJSON a => FromJSON (Result a)


checkBit :: Integer -> Integer -> Bool
checkBit a b = a .&. b == b

