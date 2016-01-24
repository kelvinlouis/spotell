{-# LANGUAGE OverloadedStrings, RecordWildCards  #-}

module Youtube.PlaylistVideo
    ( getPlaylistVideos
    ) where

import Network.Wreq hiding (Response)
import Control.Lens
import Data.Aeson
import Prelude hiding (id)
import qualified Data.Text as T

data Response = Response {
  nextPageToken :: String,
  items         :: [Item]
} deriving (Show)

data Item = Item {
  id    :: String,
  title :: String
} deriving (Show)

instance FromJSON Response where
  parseJSON = withObject "response" $ \o -> do
    items         <- o .: "items"
    nextPageToken <- o .:? "nextPageToken" .!= "END"
    return Response{..}

instance FromJSON Item where
  parseJSON = withObject "item" $ \o -> do
    id      <- o .: "id"
    snippet <- o .: "snippet" 
    title   <- snippet .: "title"
    return Item{..}

getPlaylistVideos :: String -> IO ([Item])
getPlaylistVideos pid = call pid "" []

call :: String -> String -> [Item] -> IO ([Item])
call pid token is = do
  if token == "END" then
    return (is)
  else do
    let opts = defaults & param "part" .~ ["snippet"]
                        & param "playlistId" .~ [T.pack pid]
                        & param "maxResults" .~ ["10"]
                        & param "pageToken" .~ [T.pack token]
                        & param "fields" .~ ["items(id,snippet(title)),nextPageToken"]
                        & param "key" .~ ["AIzaSyClRz-XU6gAt4h-_JRdIA2UIQn8TroxTIk"]
    r <- asJSON =<< getWith opts "https://www.googleapis.com/youtube/v3/playlistItems"
    let nis = is ++ (items $ (r ^. responseBody))
    call pid (nextPageToken $ (r ^. responseBody)) nis