module Main where

import           Control.Error (Script, runExceptT)
import           System.Environment (getExecutablePath)
import           System.FilePath ((</>), takeDirectory)
import           Protolude hiding (get)
import           Web.Scotty
import           Network.Wai.Middleware.Static (addBase, staticPolicy)
import           Config
import qualified Nix
import           Types (depsToJson, makeEnv, runApp)

safeIO :: Script a -> ActionM a
safeIO script = do
  result <- liftIO $ runExceptT script
  case result of
    Right a  -> return a
    Left err -> do
      putText err
      raise . toS $ err

findStaticAssets :: StaticAssetLocation -> IO Text
findStaticAssets (Path p) = return p
findStaticAssets InstallLocation = do
  exePath <- getExecutablePath
  let installLocation = takeDirectory . takeDirectory $ exePath
  return $ toS (installLocation </> "static")

runGrafanix :: Config -> IO ()
runGrafanix config = do
  env <- makeEnv config
  staticAssets <- toS <$> findStaticAssets (staticPath config)
  putStrLn $ "Serving static assets from " <> staticAssets
  scotty (port config)
    $ do
      middleware $ staticPolicy (addBase staticAssets)
      get "/" $ file (staticAssets <> "/index.html")
      get "/deps/:packageName/"
        $ do
          pkgName <- param "packageName"
          pkgPath <- safeIO $ runApp env $ Nix.pkgPath pkgName
          (graph, infoMap, whyMap)
            <- safeIO $ runApp env $ Nix.depGraph pkgPath
          json $ depsToJson graph infoMap whyMap
      get "/build-deps/:packageName"
        $ do
          pkgName <- param "packageName"
          pkgPath <- safeIO $ runApp env $ Nix.drvPath pkgName
          (graph, infoMap, whyMap)
            <- safeIO $ runApp env $ Nix.depGraph pkgPath
          json $ depsToJson graph infoMap whyMap

main :: IO ()
main = runGrafanix =<< readConfig
