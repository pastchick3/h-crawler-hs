module Lib (crawl) where

import Config
import Control.Monad
import Control.Monad.Trans.Reader
import Data.List
import Fanbox qualified

crawl :: Config -> [String] -> IO ()
crawl cfg urls = forM_ urls process
  where
    process url
      | Just path <- stripPrefix "https://www.fanbox.cc" url = runReaderT (Fanbox.crawl path) cfg
      | otherwise = error $ "Unknown Website: " ++ url
