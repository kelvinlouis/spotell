{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Youtube.Playlist
    ( getPlaylists
    ) where

import Network.Wreq
import Control.Lens
import GHC.Generics
import Data.Aeson
import Prelude hiding (id)
import qualified Data.Text as T

data YTPlaylistSnippet = YTPlaylistSnippet {
  id          :: String,
  title       :: String,
  description :: String }
  deriving (Show,Generic)

data YTPlaylistSnippetArray = YTPlaylistSnippetArray {
  items :: [YTPlaylistSnippet] }
  deriving (Show,Generic)

instance FromJSON YTPlaylistSnippet
instance FromJSON YTPlaylistSnippetArray

getPlaylists :: String -> IO (String)
getPlaylists cid = do
  let opts = defaults & param "part" .~ ["snippet"]
                      & param "channelId" .~ [T.pack cid]
                      & param "maxResults" .~ ["50"]
                      & param "key" .~ ["AIzaSyClRz-XU6gAt4h-_JRdIA2UIQn8TroxTIk"]
  r <- getWith opts "https://www.googleapis.com/youtube/v3/playlists"
  return ("ASD")