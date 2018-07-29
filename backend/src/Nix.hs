module Nix
  ( Script
  , drvPath
  , pkgPath
  , depTree
  )
where

import           Control.Error                  ( Script
                                                , scriptIO
                                                )
import           Data.Attoparsec.Text           ( Parser
                                                , parseOnly
                                                )
import qualified Data.ByteString.Lazy as L
import           Data.Hashable                  ( Hashable )
import           Data.IORef                     ( atomicModifyIORef )
import           Data.LruCache                  ( insert
                                                , lookup
                                                )
import           Data.LruCache.IO               ( LruHandle(..) )
import           Data.Tree                      ( Tree(..) )
import           Data.Text                      ( isSuffixOf
                                                , strip
                                                , unwords
                                                )
import           Regex.RE2                      ( compile
                                                , replaceAll
                                                )

import           System.Environment             ( getEnv )
import           System.Process.Typed
import           Protolude

import           Config
import qualified Parser
import           Types

decolor :: ByteString -> ByteString
decolor str = fst $ replaceAll colors str ""
  where Right colors = compile "\\033\\[(\\d+;)*\\d+m"

run :: Text -> [Text] -> Script ByteString
run cmd args = do
  putText cmdline
  let
    procConfig = setStdin closed
               $ setStdout byteStringOutput
               $ setStderr closed
               $ proc (toS cmd) (map toS args)
  (exitCode, out, err) <- readProcess procConfig
  if exitCode == ExitSuccess
    then do
      let decolored = decolor . L.toStrict $ out
      return decolored
    else
      let message = "Command '" <> cmdline <> "' failed with:\n" <> toS err
      in  throwError message
  where cmdline = cmd <> " " <> unwords args

cached
  :: (Hashable k, Ord k) => LruHandle k v -> (k -> Script v) -> k -> Script v
cached (LruHandle ref) script k = do
  cachedValue <-
    scriptIO $ atomicModifyIORef ref $ \cache -> case lookup k cache of
      Nothing          -> (cache, Nothing)
      Just (v, cache') -> (cache', Just v)
  case cachedValue of
    Just v  -> return v
    Nothing -> do
      v <- script k
      scriptIO $ atomicModifyIORef ref $ \cache -> (insert k v cache, ())
      return v

parse :: Parser a -> Text -> Script a
parse parser text = case parseOnly parser text of
  Right a   -> return a
  Left  err -> throwError . toS $ err

drvPath :: Text -> App Text
drvPath pkgName = do
  nixpkgs <- asks (nixpkgsPath . config)
  out <- lift $ run "nix-instantiate" [nixpkgs, "--attr", pkgName]
  lift $ parse Parser.nixPath (toS out)

pkgPath :: Text -> App Text
pkgPath pkgName = do
  nixpkgs <- asks (nixpkgsPath . config)
  out <- lift $ run "nix-build" [nixpkgs, "--attr", pkgName, "--no-out-link"]
  lift $ parse Parser.nixPath (toS out)

sizeBytes :: Text -> Script Int
sizeBytes path = do
  out <- run "du" ["-bs", path]
  parse Parser.size (toS out)

whyDepends :: (Text, Text) -> Script [Why]
whyDepends (src, dest) = do
  out <- run "nix" ["why-depends", toS src, toS dest]
  parse Parser.whyDepends (toS out)

depTree
  :: Text
  -> App DepTree
depTree path = do
  out      <- lift $ run "nix-store" ["--query", "--tree", toS path]
  pathTree <- lift $ parse Parser.depTree (toS out)
  DepTree <$> mkNodes Nothing pathTree
 where
  buildDeps :: Bool
  buildDeps = ".drv" `isSuffixOf` toS path

  mkNodes :: Maybe Text -> Tree Text -> App (Tree Dep)
  mkNodes parent Node {..} = do
    duCache <- asks duCache
    whyCache <- asks whyCache
    size        <- lift $ cached duCache sizeBytes rootLabel
    (sha, name) <- lift $ parse Parser.hashAndName (toS rootLabel)
    why         <- if buildDeps
      then return []
      else case parent of
        Nothing -> return []
        Just p  -> lift $ cached whyCache whyDepends (p, rootLabel)

    children <- mapM (mkNodes (Just rootLabel)) subForest
    return Node {rootLabel = Dep {..}, subForest = children}
