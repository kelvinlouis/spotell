{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Youtube.PlaylistVideo
    ( getPlaylistVideos
    ) where

import Network.Wreq
import Control.Lens
import GHC.Generics
import Data.Aeson
import Prelude hiding (id)
import qualified Data.Text as T

data YTItem = YTItem {
  id      :: String,
  snippet :: YTSnippet }
  deriving (Show,Generic)

data YTSnippet = YTSnippet {
  title :: String }
  deriving (Show,Generic)

data YTItemArray = YTItemArray {
  items :: [YTItem] }
  deriving (Show,Generic)

instance FromJSON YTItem
instance FromJSON YTSnippet
instance FromJSON YTItemArray

getPlaylistVideos :: String -> IO ()
getPlaylistVideos pid = do
  let opts = defaults & param "part" .~ ["snippet"]
                      & param "playlistId" .~ [T.pack pid]
                      & param "maxResults" .~ ["50"]
                      & param "fields" .~ ["items(id,snippet(title))"]
                      & param "key" .~ ["AIzaSyClRz-XU6gAt4h-_JRdIA2UIQn8TroxTIk"]
  r <- asJSON =<< getWith opts "https://www.googleapis.com/youtube/v3/playlistItems"
  putStrLn $ show (items $ (r ^. responseBody))