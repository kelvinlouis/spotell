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

data YTChannelItem = YTChannelItem { 
  id :: String } 
  deriving (Show, Generic)

data YTChannelItemArray = YTChannelItemArray {
  items :: [YTChannelItem] } 
  deriving (Show, Generic)

instance FromJSON YTChannelItem
instance FromJSON YTChannelItemArray

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