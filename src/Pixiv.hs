{-# LANGUAGE OverloadedStrings #-}

module Pixiv (crawl) where

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
import Text.Regex.TDFA

crawl :: String -> App ()
crawl path = do
  let (_, _, art_id) :: (String, String, String) = path =~ ("/artworks/" :: String)
  info <- requestInfo art_id
  let (name, num, url) = (fromJust . processInfo) info
  if num == 1
  then do
    liftIO $ putStr $ "\r" ++ name ++ " - 0/1"
    liftIO $ hFlush stdout
    (image, ext) <- requestImage url
    liftIO $ BS.writeFile (name <.> ext) image
    liftIO $ putStr $ "\r" ++ name ++ " - 1/1"
    liftIO $ hFlush stdout
  else do
    pages <- requestPages art_id
    let urls = (fromJust . processPages) pages
    images <- forM urls requestImage
    liftIO $ createDirectory name
    forM_ (zip [1..] images) \(i, (image, ext)) -> do
      let s = show (i :: Int)
      let counter = replicate (4 - length s) '0' ++ s
      liftIO $ BS.writeFile (name </> counter <.> ext) image
      liftIO $ putStr $ "\r" ++ name ++ " - " ++ s ++ "/" ++ show num
      liftIO $ hFlush stdout
  liftIO $ putStrLn ""

buildReq :: String -> App Request
buildReq url = do
  cookie <- asks (.pixiv.phpsessid)
  pure $ addRequestHeader "Cookie" (BS.pack $ "PHPSESSID=" ++ cookie)
    $ addRequestHeader "Referer" "https://www.pixiv.net/"
    $ addRequestHeader "User-Agent" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/26.6 Safari/605.1.15"
    $ parseRequest_ url

requestInfo :: String -> App Object
requestInfo art_id = do
  req <- buildReq $ "https://www.pixiv.net/ajax/illust/" ++ art_id
  resp <- liftIO $ httpJSON req
  pure $ getResponseBody resp

processInfo :: Object -> Maybe (String, Int, String)
processInfo = parseMaybe \info -> do
  body <- info .: "body"
  author <- body .: "userName"
  datetime :: String <- body .: "createDate"
  let date = (drop 2) . (filter (/= '-')) $ datetime =~ ("^20..-..-.." :: String)
  title <- body .: "title"
  let name = "[" ++ author ++ "] [" ++ date ++ "] " ++ title
  num <- body .: "pageCount"
  url <- body .: "urls" >>= (.: "original")
  pure (name, num, url)

requestPages :: String -> App Object
requestPages art_id = do
  req <- buildReq $ "https://www.pixiv.net/ajax/illust/" ++ art_id ++ "/pages"
  resp <- liftIO $ httpJSON req
  pure $ getResponseBody resp

processPages :: Object -> Maybe [String]
processPages = parseMaybe \pages -> do
  body <- pages .: "body"
  forM body \p -> p .: "urls" >>= (.: "original")

requestImage :: String -> App (BS.ByteString, String)
requestImage url = do
  req <- buildReq url
  resp <- liftIO $ httpBS req
  pure (getResponseBody resp, takeExtension url)
