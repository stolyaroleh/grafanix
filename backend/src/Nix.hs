module Nix
  ( drvPath,
    pkgPath,
    depGraph,
  )
where

import Config
import Control.Error
  ( Script,
    scriptIO,
  )
import Data.Attoparsec.Text
  ( Parser,
    parseOnly,
  )
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import Data.Cache.LRU.IO (AtomicLRU, insert, lookup)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Parser
import Protolude
import System.Process.Typed
import Types

decolor :: ByteString -> ByteString
decolor = go mempty
  where
    go acc "" = acc
    go acc string =
      let esc = '\x1b'
          colorSequenceStart = "\x1b["
          (text, colored) = C.span (/= esc) string
          -- Attempt to strip a color sequence
          rest = case C.stripPrefix colorSequenceStart colored of
            Just x ->
              -- All color sequences look like this: ESC[#(;#)m
              C.drop 1 . C.dropWhile (/= 'm') $ x
            Nothing ->
              -- Just skip ESC otherwise
              C.dropWhile (== esc) colored
       in go (acc <> text) rest

run :: Text -> [Text] -> Script Text
run cmd args = do
  putText cmdline
  let procConfig =
        setStdin closed $
          setStdout byteStringOutput $
            setStderr closed $
              proc
                (toS cmd)
                (map toS args)
  (exitCode, out, err) <- readProcess procConfig
  if exitCode == ExitSuccess
    then return . decodeUtf8 . decolor . L.toStrict $ out
    else
      let message = "Command '" <> cmdline <> "' failed with:\n" <> (decodeUtf8 . L.toStrict $ err)
       in throwError message
  where
    cmdline = cmd <> " " <> Text.unwords args

cached ::
  (Hashable k, Ord k) => AtomicLRU k v -> (k -> Script v) -> k -> Script v
cached cache script k = do
  cachedValue <- scriptIO $ lookup k cache
  case cachedValue of
    Just v -> return v
    Nothing -> do
      v <- script k
      scriptIO $ insert k v cache
      return v

parse :: Parser a -> Text -> Script a
parse parser text = case parseOnly parser text of
  Right a -> return a
  Left err -> throwError . toS $ err

drvPath :: Text -> App Text
drvPath pkgExpr = do
  nixpkgs <- asks (nixpkgsPath . config)
  out <- lift $ run "nix-instantiate" ["--expr", "with import " <> nixpkgs <> " {}; " <> pkgExpr]
  lift $ parse Parser.nixPath out

pkgPath :: Text -> App Text
pkgPath pkgExpr = do
  nixpkgs <- asks (nixpkgsPath . config)
  out <- lift $ run "nix-build" ["--expr", "with import " <> nixpkgs <> " {}; " <> pkgExpr, "--no-out-link"]
  lift $ parse Parser.nixPath out

sizeAndClosureSize :: Text -> Script (Int, Int)
sizeAndClosureSize path = do
  out <- run "nix" ["path-info", "--size", "--closure-size", path]
  parse Parser.sizeAndClosureSize out

whyDepends :: (Text, Text) -> Script (Vector Why)
whyDepends (src, dest) = do
  out <- run "nix" ["why-depends", "--all", src, dest]
  parse Parser.whyDepends out

info :: Text -> App Info
info path = do
  sizeCache <- asks sizeCache
  (size, closureSize) <- lift $ cached sizeCache sizeAndClosureSize path
  (sha, name) <- lift $ parse Parser.hashAndName path
  return Info {..}

depGraph :: Text -> App (DepGraph, Map Int Info, Map (Int, Int) (Vector Why))
depGraph path = do
  out <- lift $ run "nix-store" ["--query", "--graph", path]
  graph <- lift $ parse Parser.depGraph out
  let DepGraph {..} = graph
  infoVector <- mapM info nodes
  let infoMap = vectorToMap . Vector.indexed $ infoVector
  let textEdge (srcIndex, destIndex) = (nodes Vector.! srcIndex, nodes Vector.! destIndex)
      textEdges = Vector.map textEdge edges
  whyVector <- mapM getWhy textEdges
  let whyMap = vectorToMap $ Vector.zip edges whyVector
  return (graph, infoMap, whyMap)
  where
    vectorToMap :: Ord a => Vector (a, b) -> Map a b
    vectorToMap = Map.fromList . Vector.toList

    getWhy :: (Text, Text) -> App (Vector Why)
    getWhy (src, dest) = do
      whyCache <- asks whyCache
      lift $ cached whyCache whyDepends (src, dest)
