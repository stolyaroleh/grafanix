module Config (Config(..), StaticAssetLocation(..), devConfig, readConfig) where

import           Data.String (fromString)
import           Options.Applicative
import           Protolude hiding (option)

data StaticAssetLocation = InstallLocation
                         | Path Text
  deriving (Show)

instance IsString StaticAssetLocation where
  fromString = Path . toS

data Config = Config { nixpkgsPath :: Text
                     , staticPath :: StaticAssetLocation
                     , duCacheSize :: Integer
                     , whyCacheSize :: Integer
                     , port :: Int
                     }
  deriving (Show)

devConfig :: Config
devConfig = Config { nixpkgsPath = "<nixpkgs>"
                   , staticPath = Path "../static"
                   , duCacheSize = 4096
                   , whyCacheSize = 2048
                   , port = 3000
                   }

config :: Parser Config
config = Config
  <$> strOption
    (long "pkgs-path"
     <> help "Package set path"
     <> showDefault
     <> value (nixpkgsPath devConfig))
  <*> option
    str
    (long "static-path"
     <> help "Path to serve static assets from"
     <> showDefault
     <> value InstallLocation)
  <*> pure (duCacheSize devConfig)
  <*> pure (whyCacheSize devConfig)
  <*> option
    auto
    (long "port"
     <> help "Port to listen on"
     <> showDefault
     <> value (port devConfig))

readConfig :: IO Config
readConfig = execParser opts
  where
    opts = info
      (config <**> helper)
      (fullDesc <> header "Grafanix - Visualize your Nix dependencies")
