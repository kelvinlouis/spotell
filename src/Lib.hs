{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( searchYTPlaylists
    ) where

import Data.Maybe
import Data.List.Split
import Youtube.Channel
import Youtube.Playlist
import Youtube.PlaylistVideo

extract :: Maybe a -> a
extract (Just x) = x
extract Nothing = error "There is nothing to extract from (Maybe a)"

hr :: IO()
hr = putStrLn "======================================================================"

searchYTPlaylists :: IO()
searchYTPlaylists = do
  hr
  putStrLn "Enter name of Youtube channel:"
  channelName <- return ("worldstarhiphoptv")
  putStrLn channelName
  hr
  cid <- getChannelId $ channelName
  if (isJust cid) then
    do
      playlists <- getPlaylists $ extract cid
      sequence $ listPlaylists playlists 1
      hr
      putStrLn "Enter # of playlist you're interested in (only one):"
      --nr <- getLine
      nr <- return ("3")
      --nr <- return ("16")
      putStrLn nr
      items <- getPlaylistVideos (playlistId playlists (read nr :: Int))
      hr
      putStrLn $ show $ getTitles items
      return ()
  else
    searchYTPlaylists

listPlaylists :: [Playlist] -> Int -> [IO ()]
listPlaylists [] _ = [return ()]
listPlaylists ((Playlist _ title):ps) nr = line : (listPlaylists ps (nr+1))
                        where line = putStrLn ("#" ++ (show nr) ++ " " ++ title)

playlistId :: [Playlist] -> Int -> String
playlistId pl idx = pid (pl!!(idx-1))

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