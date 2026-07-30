{-# LANGUAGE OverloadedStrings #-}

module Fanbox (crawl) where

import Config
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Char8 qualified as BS
import Data.Maybe
import Network.HTTP.Simple
import System.Directory
import System.FilePath
import System.IO
import System.Process
import Text.Regex.TDFA

crawl :: String -> App ()
crawl path = do
  let (_, _, post_id) :: (String, String, String) = path =~ ("/posts/" :: String)
  info <- requestInfo post_id
  let (name, urls) = (fromJust . processInfo . fromJust . decodeStrict . BS.pack) info
  let num = length urls
  liftIO $ putStr $ "\r" ++ name ++ " - 0/" ++ show num
  liftIO $ hFlush stdout
  if num == 1
  then do
    (image, ext) <- requestImage $ urls !! 0
    liftIO $ BS.writeFile (name <.> ext) image
    liftIO $ putStr $ "\r" ++ name ++ " - 1/1"
    liftIO $ hFlush stdout
  else do
    images <- forM urls requestImage
    liftIO $ createDirectory name
    forM_ (zip [1..] images) \(i, (image, ext)) -> do
      let s = show (i :: Int)
      let counter = replicate (4 - length s) '0' ++ s
      liftIO $ BS.writeFile (name </> counter <.> ext) image
      liftIO $ putStr $ "\r" ++ name ++ " - " ++ s ++ "/" ++ show num
      liftIO $ hFlush stdout
  liftIO $ putStrLn ""

requestInfo :: String -> App String
requestInfo post_id = do
  cookie <- asks (.fanbox.fanboxsessid)
  liftIO $ readProcess "./curl-impersonate" [
    "--no-progress-meter",
    "-H", "Origin: https://www.fanbox.cc",
    "-H", "Cookie: FANBOXSESSID=" ++ cookie,
    "https://api.fanbox.cc/post.info?postId=" ++ post_id] []

processInfo :: Object -> Maybe (String, [String])
processInfo = parseMaybe \info -> do
  post <- info .: "body" >>= (.: "post")
  author <- post .: "user" >>= (.: "name")
  datetime :: String <- post .: "publishedDatetime"
  let date = (drop 2) . (filter (/= '-')) $ datetime =~ ("^20..-..-.." :: String)
  title <- post .: "title"
  let name = "[" ++ author ++ "] [" ++ date ++ "] " ++ title

  body <- post .: "body"
  blocks <- body .: "blocks"
  images <- flip filterM blocks \block -> do
    ty :: String <- block .: "type"
    pure $ ty == "image"
  image_ids <- forM images (.: "imageId")
  image_map <- body .: "imageMap"
  urls <- forM image_ids \image_id -> image_map .: image_id >>= (.: "originalUrl")

  pure (name, urls)

requestImage :: String -> App (BS.ByteString, String)
requestImage url = do
  cookie <- asks (.fanbox.fanboxsessid)
  let req = addRequestHeader "Cookie" (BS.pack $ "FANBOXSESSID=" ++ cookie)
          $ parseRequest_ url
  resp <- liftIO $ httpBS req
  pure (getResponseBody resp, takeExtension url)
