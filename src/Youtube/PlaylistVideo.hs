{-# LANGUAGE OverloadedStrings, RecordWildCards  #-}

module Youtube.PlaylistVideo
    ( getPlaylistVideos
    ) where

import Network.Wreq hiding (Response)
import Control.Lens
import Data.Aeson
import Prelude hiding (id)
import qualified Data.Text as T

data Item = Item {
  id    :: String,
  title :: String
} deriving (Show)

data Response = Response {
  nextPageToken :: String,
  items         :: [Item]
} deriving (Show)

instance FromJSON Response where
  parseJSON = withObject "response" $ \o -> do
    items         <- o .: "items"
    nextPageToken <- o .:? "nextPageToken" .!= ""
    return Response{..}

instance FromJSON Item where
  parseJSON = withObject "item" $ \o -> do
    id      <- o .: "id"
    snippet <- o .: "snippet" 
    title   <- snippet .: "title"
    return Item{..}

getPlaylistVideos :: String -> IO ()
getPlaylistVideos pid = do
  let opts = defaults & param "part" .~ ["snippet"]
                      & param "playlistId" .~ [T.pack pid]
                      & param "maxResults" .~ ["50"]
                      & param "fields" .~ ["items(id,snippet(title)),nextPageToken"]
                      & param "key" .~ ["AIzaSyClRz-XU6gAt4h-_JRdIA2UIQn8TroxTIk"]
  r <- asJSON =<< getWith opts "https://www.googleapis.com/youtube/v3/playlistItems"
  putStrLn $ show (items $ (r ^. responseBody))