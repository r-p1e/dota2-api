{-# LANGUAGE DeriveGeneric #-}

module Dota2.Types where

import           Data.Aeson       (FromJSON (..), genericParseJSON)
import           Data.Aeson.Types (Options (..), camelTo2, defaultOptions)
import           Data.Text        (Text)
import           GHC.Generics     (Generic)
import           Web.HttpApiData  (ToHttpApiData (..))

newtype SteamKey =
    SteamKey Text
    deriving (Generic)

instance ToHttpApiData SteamKey where
  toQueryParam (SteamKey sk) = sk

newtype Language = Language Text deriving Generic

instance ToHttpApiData Language where
  toQueryParam (Language lang) = lang


data Leagues = Leagues
    { lName          :: Text
    , lLeagueId      :: Text
    , lDescription   :: Text
    , lTournamentUrl :: Text
    } deriving (Generic)

instance FromJSON Leagues where
    parseJSON =
        genericParseJSON
            defaultOptions
            { fieldLabelModifier = camelTo2 '_' . drop 1
            }


data Result a = Result
    { result :: a
    } deriving Generic

instance FromJSON a => FromJSON (Result a)
