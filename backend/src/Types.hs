module Types
  ( App,
    Env (..),
    Info (..),
    Why (..),
    DepGraph (..),
    emptyGraph,
    depsToJson,
    makeEnv,
    runApp,
  )
where

import Config
import Control.Error (Script)
import Data.Aeson
  ( ToJSON,
    Value,
    object,
    toJSON,
  )
import Data.Cache.LRU.IO
  ( AtomicLRU,
    newAtomicLRU,
  )
import qualified Data.Map as Map
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Generics
import Protolude

type App = ReaderT Env Script

runApp :: Env -> App a -> Script a
runApp = flip runReaderT

-- Global application state
-- Since we are only interested in /nix/store and the store is immutable,
-- it is safe to cache information about store paths.
data Env = Env
  { config :: Config,
    -- Cache storing sizes and closure sizes.
    sizeCache :: AtomicLRU Text (Int, Int),
    -- Cache storing reasons why there is a dependency between two store paths (src, dest).
    whyCache :: AtomicLRU (Text, Text) (Vector Why)
  }

makeEnv :: Config -> IO Env
makeEnv config = do
  sizeCache <- newAtomicLRU (Just . duCacheSize $ config)
  whyCache <- newAtomicLRU (Just . whyCacheSize $ config)
  return Env {..}

-- A node in a dependency tree
data Info = Info
  { name :: Text,
    sha :: Text,
    size :: Int,
    closureSize :: Int
  }
  deriving (Eq, Show)

-- A reason why a node depends on its parent
data Why = Why
  { file :: Text,
    reason :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON Why

data DepGraph = DepGraph
  { nodes :: Vector Text,
    edges :: Vector (Int, Int)
  }
  deriving (Show)

emptyGraph :: DepGraph
emptyGraph = DepGraph {nodes = Vector.empty, edges = Vector.empty}

depsToJson :: DepGraph -> Map Int Info -> Map (Int, Int) (Vector Why) -> Value
depsToJson graph infos whys =
  object
    [ ("nodes", toJSON . Vector.imapMaybe mkNode $ nodes graph),
      ("links", toJSON . Vector.mapMaybe mkLink $ edges graph)
    ]
  where
    mkNode :: Int -> Text -> Maybe Value
    mkNode n _ = do
      Info {..} <- infos Map.!? n
      return $
        object
          [ ("name", toJSON name),
            ("size", toJSON size),
            ("sha", toJSON sha),
            ("closureSize", toJSON closureSize)
          ]
    mkLink :: (Int, Int) -> Maybe Value
    mkLink (sourceIndex, targetIndex) = do
      why <- whys Map.!? (sourceIndex, targetIndex)
      return $
        object
          [ ("source", toJSON sourceIndex),
            ("target", toJSON targetIndex),
            ("why", toJSON why)
          ]
