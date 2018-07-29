module Main where

import           Control.Error                  ( runExceptT )
import           Data.LruCache.IO               ( newLruHandle )
import           Protolude               hiding ( get )
import           Web.Scotty
import           Network.Wai.Middleware.Static (addBase, staticPolicy)

import           Config (Config(..), readConfig)
import           Nix
import           Types (Env(..), runApp)

safeIO :: Script a -> ActionM a
safeIO script = do
  result <- liftIO $ runExceptT script
  case result of
    Right a   -> return a
    Left  err -> do
      putText err
      raise . toS $ err

main :: IO ()
main = do
  config <- readConfig "./config.dhall"
  duCache <- newLruHandle (fromIntegral . duCacheSize $ config)
  whyCache <- newLruHandle (fromIntegral . whyCacheSize $ config)
  let env = Env {..}
  scotty 3000 $ do
    middleware $ staticPolicy (addBase . toS $ staticPath config)
    get "/" $ file . toS $ staticPath config <> "/index.html"
    get "/deps/:packageName/" $ do
      pkgName <- param "packageName"
      deps    <- safeIO $ runApp env $ depTree =<< pkgPath pkgName
      json deps
    get "/build-deps/:packageName" $ do
      pkgName <- param "packageName"
      deps    <- safeIO $ runApp env $ depTree =<< drvPath pkgName
      json deps
