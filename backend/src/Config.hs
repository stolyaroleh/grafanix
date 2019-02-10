module Config
  (Config(..), readConfig)
where

import Dhall (Interpret, auto, input)
import GHC.Generics
import Protolude

data Config = Config
  { nixpkgsPath :: Text
  , staticPath :: Text
  , duCacheSize :: Integer
  , whyCacheSize :: Integer
  } deriving (Show, Generic)

instance Interpret Config

readConfig :: FilePath -> IO Config
readConfig file = input auto (toS file)
