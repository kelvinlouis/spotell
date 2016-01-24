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

data YTPlaylistItem = YTPlaylistItem {
  id          :: String,
  snippet     :: YTPlaylistSnippet }
  deriving (Show,Generic)

data YTPlaylistSnippet = YTPlaylistSnippet {
  title       :: String,
  description :: String }
  deriving (Show,Generic)

data YTPlaylistItemArray = YTPlaylistItemArray {
  items :: [YTPlaylistItem] }
  deriving (Show,Generic)

instance FromJSON YTPlaylistItem
instance FromJSON YTPlaylistSnippet
instance FromJSON YTPlaylistItemArray

type YTPlaylist = (String, String)

simplify :: YTPlaylistItem -> YTPlaylist
simplify (YTPlaylistItem id (YTPlaylistSnippet t _)) = (id, t)

getPlaylists :: String -> IO ([YTPlaylist])
getPlaylists cid = do
  let opts = defaults & param "part" .~ ["snippet"]
                      & param "channelId" .~ [T.pack cid]
                      & param "maxResults" .~ ["50"]
                      & param "fields" .~ ["items(id,snippet(title,description))"]
                      & param "key" .~ ["AIzaSyClRz-XU6gAt4h-_JRdIA2UIQn8TroxTIk"]
  r <- asJSON =<< getWith opts "https://www.googleapis.com/youtube/v3/playlists"
  return (map simplify (items $ (r ^. responseBody)))