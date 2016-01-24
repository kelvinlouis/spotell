{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Lib
    ( searchYTPlaylists
    ) where

import GHC.Generics
import Data.Maybe
import Youtube.Channel
import Youtube.Playlist

extract :: Maybe a -> a
extract (Just x) = x
extract Nothing = error "There is nothing to extract from (Maybe a)"

searchYTPlaylists :: IO()
searchYTPlaylists = do
  putStrLn "Enter name of Youtube channel:"
  channelName <- getLine
  cid <- getChannelId $ channelName
  if (isJust cid) then
    do
      playlists <- getPlaylists $ extract cid
      putStrLn playlists
      return ()
  else
    searchYTPlaylists

--getYTVideos :: IO()
--getYTVideos = do
--  let opts = defaults & param "part" .~ ["snippet"]
--                      & param "channelId" .~ ["UC-yXuc1__OzjwpsJPlxYUCQ"]
--                      & param "sort" .~ ["date"]
--                      & param "type" .~ ["video"]
--                      & param "maxResults" .~ ["50"]
--                      & param "key" .~ ["AIzaSyClRz-XU6gAt4h-_JRdIA2UIQn8TroxTIk"]
--  r <- getWith opts "https://www.googleapis.com/youtube/v3/search"
--  putStrLn $ show r

--getYoutubePlaylistItems :: IO()
--getYoutubePlaylistItems = do
--  let opts = defaults & param "part" .~ ["snippet"]
--                      & param "playlistId" .~ ["PLcK0neBMyFxSpYgDfKCsHRwgxlN-Tnt9D"]
--                      & param "key" .~ ["AIzaSyClRz-XU6gAt4h-_JRdIA2UIQn8TroxTIk"]
--  r <- getWith opts "https://www.googleapis.com/youtube/v3/playlistItems"
--  putStrLn $ show r