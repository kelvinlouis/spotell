{-# LANGUAGE OverloadedStrings, RecordWildCards  #-}

module Youtube.PlaylistVideo
    ( getPlaylistVideos,
      getTitles
    ) where

import Network.Wreq hiding (Response)
import Control.Lens
import Data.Aeson
import System.Directory
import Prelude hiding (id)
import qualified Data.Text as T

data Response = Response {
  nextPageToken :: String,
  items         :: [Item]
} deriving (Show)

data Item = Item {
  id    :: String,
  title :: String
} deriving (Show, Read)

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

data CacheEntry = CacheEntry {
  cid     :: String,
  created :: String,
  objects :: [Item]
} deriving (Show, Read)

cacheFilePath :: String
cacheFilePath = "playlist.video.cache"

getPlaylistVideos :: String -> IO ([Item])
getPlaylistVideos pid = do
  cachedEntries <- readCache
  if playlistExists pid cachedEntries then do
    return (getCachedItems pid cachedEntries)
  else do
    items <- call pid "" []
    cache pid items
    return (items)

call :: String -> String -> [Item] -> IO ([Item])
call pid token is = do
  if token == "END" then
    return (is)
  else do
    let opts = defaults & param "part" .~ ["snippet"]
                        & param "playlistId" .~ [T.pack pid]
                        & param "maxResults" .~ ["50"]
                        & param "pageToken" .~ [T.pack token]
                        & param "fields" .~ ["items(id,snippet(title)),nextPageToken"]
                        & param "key" .~ ["AIzaSyClRz-XU6gAt4h-_JRdIA2UIQn8TroxTIk"]
    r <- asJSON =<< getWith opts "https://www.googleapis.com/youtube/v3/playlistItems"
    let nis = is ++ (items $ (r ^. responseBody))
    call pid (nextPageToken $ (r ^. responseBody)) nis

cache :: String -> [Item] -> IO ()
cache pid list = do
  existingCache <- readCache
  if expired pid existingCache then do
    writeCache $ (CacheEntry pid "" list):existingCache
    return ()
  else do
    writeCache $ (CacheEntry pid "" list):existingCache
    return ()

expired :: String -> [CacheEntry] -> Bool
expired nid [] = False
expired nid ((CacheEntry eid _ _ ):xs) | nid == eid = True
                                       | otherwise = expired nid xs

readCache :: IO [CacheEntry]
readCache = do
  fileExists <- doesFileExist cacheFilePath
  if not fileExists then do
    writeFile cacheFilePath ""
    return ([])
  else do
    content <- readFile cacheFilePath
    if null content then
      return ([])
    else do
      return (read content)

writeCache :: [CacheEntry] -> IO ()
writeCache list = do
  writeFile cacheFilePath (show list)

playlistExists :: String -> [CacheEntry] -> Bool
playlistExists pid = foldr check False
  where check (CacheEntry i _ _) acc | (acc == False && i == pid) = True
                                     | otherwise = False

getCachedItems :: String -> [CacheEntry] -> [Item]
getCachedItems pid = foldr extract []
  where extract (CacheEntry i _ ls) acc | i == pid = ls ++ acc
                                        | otherwise = acc

getTitles :: [Item] -> [String]
getTitles = map title