{-# LANGUAGE BinaryLiterals        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Dota2.Types where

import           Data.Aeson       (FromJSON (..), genericParseJSON, (.:))
import           Data.Aeson.Types (Options (..), Value (Number, Object),
                                   camelTo2, defaultOptions, typeMismatch)
import           Data.Bits        (Bits ((.&.)))
import           Data.Text        (Text)
import           Data.Time        (UTCTime)
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

type LeagueId = Text

data League = League
    { name          :: Text  -- The name of the league
    , leagueId      :: LeagueId  -- The league's unique ID
    , description   :: Text  -- A description of the league
    , tournamentUrl :: Text  -- The league's website
    } deriving (Generic)

instance FromJSON League where
    parseJSON =
        genericParseJSON
            defaultOptions
            { fieldLabelModifier = camelTo2 '_'
            }

data Games = Games
    { games :: [Game]  -- A list of games
    } deriving (Generic)

instance FromJSON Games

type LobbyId = Text

data Game = Game
    { players     :: [Player]  -- The list of players within a game
    , radiantTeam :: Team  -- Information about the team playing as radiant for this match
    , direTeam    :: Team  -- Information about the team playing as dire for this match
    , lobbyId     :: LobbyId  -- Unique ID for the matches lobby.
    , spectators  :: Int  -- Number of spectators at time of query
    , towerState  :: TowerState  -- See 'TowerState' below
    , leagueId    :: LeagueId-- Unique ID for the league of the match being played. A list of league IDs can be found via the 'getLeagueListing' method.
    } deriving (Generic)

instance FromJSON Game where
    parseJSON =
        genericParseJSON
            defaultOptions
            { fieldLabelModifier = camelTo2 '_'
            }

type AccountId = Integer
type HeroId = Text
type ItemId = Text

-- | A player's slot is returned via an 8-bit unsigned integer.
-- The first bit represent the player's team, false if Radiant and
-- true if dire. The final three bits represent the player's position
-- in that team, from 0-4.
data PlayerSlot = PlayerSlot
    { teamType :: TeamType  -- team Radiant or Dire
    , position :: Int  -- The position of a player within their team (0-4)
    }

instance FromJSON PlayerSlot where
    parseJSON v@(Number n) =
        let extractTeamType ps =
                if checkBit ps 0b10000000
                    then Dire
                    else Radiant
            extractPosition ps
              | checkBit ps 0b100 = 4
              | checkBit ps 0b11 = 3
              | checkBit ps 0b10 = 2
              | checkBit ps 0b1 = 1
              | otherwise = 0
        in case Scientific.floatingOrInteger n of
               Right i ->
                   return
                       PlayerSlot
                       { teamType = extractTeamType i
                       , position = extractPosition i
                       }
               Left r ->
                   typeMismatch
                       "PlayerSlot must be an 8-bit integer, not a floating point"
                       v
    parseJSON invalid = typeMismatch "type mismatch PlayerSlot" invalid

data LeaverStatus
    = None  -- finished match, no abandon
    | Disconnected  -- player DC, no abandon
    | DisconnectedTooLong  -- player DC > 5min, abandoned
    | Abandoned  -- player DC, clicked leave, abandoned
    | AFK  -- player AFK, abanndoned
    | NeverConnected  -- player never connected, no abandon
    | NeverConnectedTooLong  -- player took too long to connect, no abandon.

instance FromJSON LeaverStatus where
    parseJSON v@(Number n) =
        case n of
            0 -> return None
            1 -> return Disconnected
            2 -> return DisconnectedTooLong
            3 -> return Abandoned
            4 -> return AFK
            5 -> return NeverConnected
            6 -> return NeverConnectedTooLong
            _ -> typeMismatch "Unknown code for LeaverStatus" v
    parseJSON invalid = typeMismatch "type mismatch LeaverStatus" invalid

type AbilityId = Text

data Ability = Ability
    { ability :: AbilityId  -- ID of abiliity upgraded
    , time    :: Int  -- Time since match start that the ability was upgraded.
    , level   :: Int  -- The level of the player at time of upgrading.
    } deriving (Generic)

instance FromJSON Ability

data Unit = Unit
    { unitname :: Text  -- The name of the units owned by the player
    , item0    :: Maybe ItemId  -- ID of the top-left inventory item.
    , item1    :: Maybe ItemId  -- ID of the top-center inventory item.
    , item2    :: Maybe ItemId  -- ID of the top-right inventory item.
    , item3    :: Maybe ItemId  -- ID of the bottom-left inventory item.
    , item4    :: Maybe ItemId  -- ID of the bottom-center inventory item.
    , item5    :: Maybe ItemId  -- ID of the bottom-right inventory item.
    } deriving (Generic)

instance FromJSON Unit where
    parseJSON =
        genericParseJSON
            defaultOptions
            { fieldLabelModifier = camelTo2 '_'
            }

data Player = Player
    { accountId       :: AccountId  -- 32-bit account ID
    , name            :: Maybe Text  -- the player's display name
    , heroId          :: HeroId  -- The hero's unique ID. A lis of hero IDs can be found via the 'getHeroes' method
    , team            :: Maybe TeamType  -- What team the player is current playing as
    , playerSlot      :: Maybe PlayerSlot
    , item0           :: Maybe ItemId  -- ID of the top-left inventory item.
    , item1           :: Maybe ItemId  -- ID of the top-center inventory item.
    , item2           :: Maybe ItemId  -- ID of the top-right inventory item.
    , item3           :: Maybe ItemId  -- ID of the bottom-left inventory item.
    , item4           :: Maybe ItemId  -- ID of the bottom-center inventory item.
    , item5           :: Maybe ItemId  -- ID of the bottom-right inventory item.
    , kills           :: Maybe Int  -- The amount of kills attributed to this player.
    , deaths          :: Maybe Int  -- The amount of times this player died during the match.
    , assists         :: Maybe Int  -- The amount of assists attributed to this player.
    , leaverStatus    :: Maybe LeaverStatus
    , gold            :: Maybe Int  -- The amount of gold the player had remaining at the end of the match.
    , lastHits        :: Maybe Int  -- The amount of last-hits the player got during the match.
    , denies          :: Maybe Int  -- The amount of denies the player got during the match.
    , goldPerMin      :: Maybe Int  -- The player's overall gold/minute.
    , xpPerMin        :: Maybe Int  -- The player's overall experience/minute
    , goldSpent       :: Maybe Int  -- The amount of gold the player spent during the match
    , heroDamage      :: Maybe Int  -- The amount of damage the player dealt to heroes
    , towerDamage     :: Maybe Int  -- The amount of damage the player dealt to towers.
    , heroHealing     :: Maybe Int  -- The amount of health the player had healed on heroes.
    , level           :: Maybe Int  -- The player's level at match end.
    , abilityUpgrades :: Maybe [Ability]  -- A list detailing a player's ability upgrades.
    , additionalUnits :: Maybe [Unit]  -- Additional playable units owned by the player.
    } deriving (Generic)


instance FromJSON Player where
    parseJSON =
        genericParseJSON
            defaultOptions
            { fieldLabelModifier = camelTo2 '_'
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

type TeamId = Text

data Team = Team
    { teamName :: Text  -- The team's name
    , teamId   :: TeamId  -- The team's unique ID.
    , teamLogo :: Text  -- The UGC id for the team logo. You can resolve this with the 'getUGCFileDetails' method
    , complete :: Bool  -- Whether the players for this team are all team members
    } deriving (Generic)

instance FromJSON Team where
    parseJSON =
        genericParseJSON
            defaultOptions
            { fieldLabelModifier = camelTo2 '_'
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

type MatchId = Text

-- | A particular teams tower status is given as an 8-bit unsigned integer
data BarracksStatus = BarracksStatus
    { bottomRanged :: Bool
    , bottomMelee  :: Bool
    , middleRanged :: Bool
    , middleMelee  :: Bool
    , topRanged    :: Bool
    , topMelee     :: Bool
    }

instance FromJSON BarracksStatus where
    parseJSON v@(Number n) =
        case Scientific.floatingOrInteger n of
            Right i ->
                return
                    BarracksStatus
                    { topMelee = checkBit i 0b1
                    , topRanged = checkBit i 0b10
                    , middleMelee = checkBit i 0b100
                    , middleRanged = checkBit i 0b1000
                    , bottomMelee = checkBit i 0b10000
                    , bottomRanged = checkBit i 0b100000
                    }
            Left r ->
                typeMismatch
                    "BarrackStatus must be 8-bit integer not floating point."
                    v
    parseJSON invalid = typeMismatch "failed when parse BarrackStatus" invalid

data TowerStatus = TowerStatus
    { ancientBottom :: Bool
    , ancientTop    :: Bool
    , bottomTier3   :: Bool
    , bottomTier2   :: Bool
    , bottomTier1   :: Bool
    , middleTier3   :: Bool
    , middleTier2   :: Bool
    , middleTier1   :: Bool
    , topTier3      :: Bool
    , topTier2      :: Bool
    , topTier1      :: Bool
    }

instance FromJSON TowerStatus where
    parseJSON v@(Number n) =
        case Scientific.floatingOrInteger n of
            Right i ->
                return
                    TowerStatus
                    { topTier1 = checkBit i 0b1
                    , topTier2 = checkBit i 0b10
                    , topTier3 = checkBit i 0b100
                    , middleTier1 = checkBit i 0b1000
                    , middleTier2 = checkBit i 0b10000
                    , middleTier3 = checkBit i 0b100000
                    , bottomTier1 = checkBit i 0b1000000
                    , bottomTier2 = checkBit i 0b10000000
                    , bottomTier3 = checkBit i 0b100000000
                    , ancientTop = checkBit i 0b1000000000
                    , ancientBottom = checkBit i 0b10000000000
                    }
            Left r ->
                typeMismatch
                    "TowerStatus must be 16-bit integer not floating point."
                    v
    parseJSON invalid = typeMismatch "failed when parse TowerStatus" invalid

data LobbyType
    = Invalid
    | PublicMatchmaking
    | Practice
    | Tournament
    | Tutorial
    | CoOpWithBots
    | TeamMatch
    | SoloQueue

instance FromJSON LobbyType where
  parseJSON (Number n) = case n of
                              -1 -> return Invalid
                              0 -> return PublicMatchmaking
                              1 -> return Practice
                              2 -> return Tournament
                              3 -> return Tutorial
                              4 -> return CoOpWithBots
                              5 -> return TeamMatch
                              6 -> return SoloQueue
                              otherwise -> return Invalid
  parseJSON invalid = typeMismatch "LobbyType" invalid

data GameMode
    = GMNone
    | AllPick
    | CaptainsMode
    | RandomDraft
    | SingleDraft
    | AllRandom
    | Intro
    | Diretide
    | ReverseCaptainMode
    | TheGreeviling
    | MDTutorial
    | MidOnly
    | LeastPlayed
    | NewPlayerPool
    | CompendiumMatchmaking
    | CaptainsDraft
    deriving Enum

instance FromJSON GameMode where
    parseJSON v@(Number n) =
        case Scientific.floatingOrInteger n of
            Right i ->
                if i > 16
                    then typeMismatch
                             "GameMode i know only 17 different modes"
                             v
                    else return (toEnum i)
            Left r -> typeMismatch "GameMode must be int" v
    parseJSON invalid = typeMismatch "GameMode" invalid

data PickBan = PickBan
    { isPick :: Bool  -- Whether this entry is a pick (True) or a ban (False).
    , heroId :: HeroId
    , team   :: TeamType  -- the team who chose the pick or ban;
    , order  :: Int  -- The order of which the picks and bans were selected; 0-19
    }

instance FromJSON PickBan where
    parseJSON (Object o) = do
        ispick <- o .: "is_pick"
        heroid <- o .: "hero_id"
        team01 <- o .: "team"
        ordr <- o .: "order"
        if team01 == (1 :: Int)
            then return
                     PickBan
                     { isPick = ispick
                     , heroId = heroid
                     , team = Dire
                     , order = ordr
                     }
            else return
                     PickBan
                     { isPick = ispick
                     , heroId = heroid
                     , team = Radiant
                     , order = ordr
                     }
    parseJSON invalid = typeMismatch "PickBan" invalid

-- | Match data type
data Match = Match
    { players               :: [Player]  -- List of players in the match
    , season                :: Int  -- The season the game was played
    , radiantWin            :: Bool  -- Dictates the winner of the match, true for radiant; false for dire.
    , duration              :: Int  -- The length of the match, in seconds since the match begin.
    , startTime             :: UTCTime  -- Unix timestamp of when the match began.
    , matchId               :: MatchId  -- The matches unique ID.
    , matchSeqNum           :: Int  -- A sequence number, represenenting the order in which matches were recorded.
    , towerStatusRadiant    :: TowerStatus
    , towerStatusDire       :: TowerStatus
    , barracksStatusRadiant :: BarracksStatus
    , barracksStatusDire    :: BarracksStatus
    , cluster               :: Text  -- The server cluster the match was played upon. Used for downloading replays of matches.
    , firstBloodTime        :: Int  -- The time in seonds since the match began when first-blood occurred.
    , lobbyType             :: LobbyType
    , humanPlayers          :: Int  -- The amount of human players within the match.
    , leagueid              :: LeagueId  -- The league that this match was a part of. A list of league IDs can be found via the 'getLeagueListing' method.}
    , positiveVotes         :: Int  -- The number of thumbs-up the game has received by users.
    , negativeVotes         :: Int  -- The number of thumbs-down the game has received by users.
    , gameMode              :: GameMode
    , picksBans             :: [PickBan]  -- A list of the picks and bans in the match, if the game mode is Captains Mode.
    , flags                 :: Text
    , engine                :: Text
    , radinatScore          :: Int
    , direScore             :: Int
    } deriving (Generic)

instance FromJSON Match where
    parseJSON =
        genericParseJSON
            defaultOptions
            { fieldLabelModifier = camelTo2 '_'
            }

-- | Common API response datatype
data Result a = Result
    { result :: a
    } deriving Generic

instance FromJSON a => FromJSON (Result a)


checkBit :: Integer -> Integer -> Bool
checkBit a b = a .&. b == b


