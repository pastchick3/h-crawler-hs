{-# LANGUAGE DerivingVia #-}

module Config (Config(..), Fanbox(..), App) where

import Control.Monad.Trans.Reader
import GHC.Generics
import Toml.Schema

data Config = Config { fanbox :: Fanbox }
  deriving (Eq, Show, Generic)
  deriving (ToTable, ToValue, FromValue) via GenericTomlTable Config

data Fanbox = Fanbox { fanboxsessid :: String }
  deriving (Eq, Show, Generic)
  deriving (ToTable, ToValue, FromValue) via GenericTomlTable Fanbox

type App = ReaderT Config IO
