module Main (main) where

import Data.Text.IO qualified as TIO
import Lib
import System.Environment
import Toml

main :: IO ()
main = do
  cfg_str <- TIO.readFile "./h-config.toml"
  let cfg = case decode cfg_str of
              Success _ c -> c
              Failure errs -> error $ "Config Errors: " ++ show errs
  urls <- getArgs
  crawl cfg urls
