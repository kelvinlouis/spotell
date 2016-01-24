{-# LANGUAGE OverloadedStrings, RecordWildCards  #-}

module Youtube.Playlist
    ( getPlaylists
    , Playlist(Playlist)
    , pid
    , title
    ) where

import Network.Wreq hiding (Response)
import Control.Lens
import Data.Aeson
import Prelude hiding (id)
import qualified Data.Text as T

data Response = Response {
  playlists :: [Playlist]
} deriving (Show)

data Playlist = Playlist {
  pid   :: String,
  title :: String
} deriving (Show)

instance FromJSON Response where
  parseJSON = withObject "response" $ \o -> do
    playlists <- o .: "items"
    return Response{..}

instance FromJSON Playlist where
  parseJSON = withObject "playlist" $ \o -> do
    pid     <- o .: "id"
    snippet <- o .: "snippet"
    title   <- snippet .: "title"
    return Playlist{..}

-- todo cache playlist with cid into txt

getPlaylists :: String -> IO ([Playlist])
getPlaylists cid = do
  let opts = defaults & param "part" .~ ["snippet"]
                      & param "channelId" .~ [T.pack cid]
                      & param "maxResults" .~ ["50"]
                      & param "fields" .~ ["items(id,snippet(title))"]
                      & param "key" .~ ["AIzaSyClRz-XU6gAt4h-_JRdIA2UIQn8TroxTIk"]
  r <- asJSON =<< getWith opts "https://www.googleapis.com/youtube/v3/playlists"
  return (playlists $ (r ^. responseBody))