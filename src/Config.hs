{-# LANGUAGE DerivingVia #-}

module Config (Config(..), Fanbox(..), Pixiv(..), App) where

import Control.Monad.Trans.Reader
import GHC.Generics
import Toml.Schema

data Config = Config { fanbox :: Fanbox, pixiv :: Pixiv }
  deriving (Eq, Show, Generic)
  deriving (ToTable, ToValue, FromValue) via GenericTomlTable Config

data Fanbox = Fanbox { fanboxsessid :: String }
  deriving (Eq, Show, Generic)
  deriving (ToTable, ToValue, FromValue) via GenericTomlTable Fanbox

data Pixiv = Pixiv { phpsessid :: String }
  deriving (Eq, Show, Generic)
  deriving (ToTable, ToValue, FromValue) via GenericTomlTable Pixiv

type App = ReaderT Config IO
