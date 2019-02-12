module Nix
  ( drvPath
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
import qualified Data.ByteString.Lazy          as L
import           Data.Hashable                  ( Hashable )
import           Data.IORef                     ( atomicModifyIORef )
import           Data.LruCache                  ( insert
                                                , lookup
                                                )
import           Data.LruCache.IO               ( LruHandle(..) )
import qualified Data.Map                      as M
import           Data.Text                      ( isSuffixOf
                                                , unwords
                                                )

import           System.Process.Typed
import           Protolude

import           Config
import qualified Parser
import           Types

run :: Text -> [Text] -> Script ByteString
run cmd args = do
  putText cmdline
  let procConfig =
        setStdin closed $ setStdout byteStringOutput $ setStderr closed $ proc
          (toS cmd)
          (map toS args)
  (exitCode, out, err) <- readProcess procConfig
  if exitCode == ExitSuccess
    then return $ L.toStrict out
    else
      let message = "Command '" <> cmdline <> "' failed with:\n" <> toS err
      in  throwError message
  where cmdline = cmd <> " " <> unwords args

cached
  :: (Hashable k, Ord k) => LruHandle k v -> (k -> Script v) -> k -> Script v
cached (LruHandle ref) script k = do
  cachedValue <- scriptIO $ atomicModifyIORef ref $ \cache ->
    case lookup k cache of
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
  out     <- lift $ run "nix-instantiate" [nixpkgs, "--attr", pkgName]
  lift $ parse Parser.nixPath (toS out)

pkgPath :: Text -> App Text
pkgPath pkgName = do
  nixpkgs <- asks (nixpkgsPath . config)
  out <- lift $ run "nix-build" [nixpkgs, "--attr", pkgName, "--no-out-link"]
  lift $ parse Parser.nixPath (toS out)

sizeAndClosureSize :: Text -> Script (Int, Int)
sizeAndClosureSize path = do
  out <- run "nix" ["path-info", "--size", "--closure-size", path]
  parse Parser.sizeAndClosureSize (toS out)

whyDepends :: (Text, Text) -> Script [Why]
whyDepends (src, dest) = do
  out <- run "nix" ["why-depends", toS src, toS dest]
  parse Parser.whyDepends (toS out)

depTree :: Text -> App (DepTree, DepInfo)
depTree path = do
  out  <- lift $ run "nix-store" ["--query", "--graph", toS path]
  tree <- lift $ parse Parser.depTree (toS out)
  let depNames = M.keys tree
  depInfos <- mapM (getInfo path) depNames
  let info = M.fromList (zip depNames depInfos)
  return (tree, info)
 where
  getInfo :: Text -> Text -> App Dep
  getInfo parent child = do
    sizeCache           <- asks sizeCache
    whyCache            <- asks whyCache
    (size, closureSize) <- lift $ cached sizeCache sizeAndClosureSize child
    (sha , name       ) <- lift $ parse Parser.hashAndName (toS child)
    why                 <- if buildDeps
      then return []
      else lift $ cached whyCache whyDepends (parent, child)
    return Dep { .. }

  buildDeps :: Bool
  buildDeps = ".drv" `isSuffixOf` toS path
