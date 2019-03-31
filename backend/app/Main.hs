module Main where

import           Control.Error                  ( Script
                                                , runExceptT
                                                )
import           Protolude               hiding ( get )
import           Web.Scotty
import           Network.Wai.Middleware.Static  ( addBase
                                                , staticPolicy
                                                )

import           Config                         ( Config(..)
                                                , readConfig
                                                )
import qualified Nix
import           Types                          ( depsToJson
                                                , makeEnv
                                                , runApp
                                                )

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
  env    <- makeEnv config
  scotty 3000 $ do
    middleware $ staticPolicy (addBase . toS $ staticPath config)
    get "/" $ file . toS $ staticPath config <> "/index.html"
    get "/deps/:packageName/" $ do
      pkgName             <- param "packageName"
      pkgPath             <- safeIO $ runApp env $ Nix.pkgPath pkgName
      (depGraph, depInfo) <- safeIO $ runApp env $ Nix.depGraph pkgPath
      json $ depsToJson depGraph depInfo
    get "/build-deps/:packageName" $ do
      pkgName             <- param "packageName"
      pkgPath             <- safeIO $ runApp env $ Nix.drvPath pkgName
      (depGraph, depInfo) <- safeIO $ runApp env $ Nix.depGraph pkgPath
      json $ depsToJson depGraph depInfo
