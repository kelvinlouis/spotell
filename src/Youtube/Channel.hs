{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Youtube.Channel
    ( getChannelId
    ) where

import Network.Wreq
import Control.Lens
import GHC.Generics
import Data.Aeson
import Data.Maybe
import Prelude hiding (id)
import qualified Data.Text as T

data ChannelItem = ChannelItem { 
  id :: String 
} deriving (Show, Generic)

data ChannelItemArray = ChannelItemArray {
  items :: [ChannelItem] 
} deriving (Show, Generic)

instance FromJSON ChannelItem
instance FromJSON ChannelItemArray

-- todo cache id with name into txt

getChannelId :: String -> IO (Maybe String)
getChannelId channelName = do
  let opts = defaults & param "part" .~ ["id"]
                      & param "forUsername" .~ [T.pack channelName]
                      & param "fields" .~ ["items/id"]
                      & param "key" .~ ["AIzaSyClRz-XU6gAt4h-_JRdIA2UIQn8TroxTIk"]
  r <- asJSON =<< getWith opts "https://www.googleapis.com/youtube/v3/channels"
  let is = items $ (r ^. responseBody)
  if length is == 1 
    then
      return (Just ((id.head) is))
    else
      return (Nothing)