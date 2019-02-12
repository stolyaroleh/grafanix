module Types
  ( App
  , Env(..)
  , Dep(..)
  , Why(..)
  , DepTree
  , DepInfo
  , depsToJson
  , makeEnv
  , runApp
  )
where

import           Control.Error                  ( Script )
import           Data.Aeson                     ( ToJSON
                                                , Value
                                                , object
                                                , toJSON
                                                )
import           Data.Map                       ( Map
                                                , (!?)
                                                , findWithDefault
                                                )
import           Data.LruCache.IO               ( LruHandle
                                                , newLruHandle
                                                )
import           GHC.Generics
import           Protolude

import           Config

type App = ReaderT Env Script

runApp :: Env -> App a -> Script a
runApp = flip runReaderT

-- Global application state
-- Since we are only interested in /nix/store and the store is immutable,
-- it is safe to cache information about store paths.
data Env = Env
  { config :: Config
    -- Cache storing sizes and closure sizes.
  , sizeCache :: LruHandle Text (Int, Int)
    -- Cache storing reasons why there is a dependency between two store paths (src, dest).
  , whyCache :: LruHandle (Text, Text) [Why]
  }

makeEnv :: Config -> IO Env
makeEnv config = do
  sizeCache <- newLruHandle (fromIntegral . duCacheSize $ config)
  whyCache  <- newLruHandle (fromIntegral . whyCacheSize $ config)
  return Env { .. }

-- A node in a dependency tree
data Dep = Dep
  { name :: Text
  , sha :: Text
  , size :: Int
  , closureSize :: Int
  , why :: [Why]
  } deriving (Eq, Show, Generic)

-- A reason why a node depends on its parent
data Why = Why
  { file :: Text
  , reason :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON Why

type DepInfo = Map Text Dep
type DepTree = Map Text [Text]

depsToJson :: DepTree -> DepInfo -> Text -> Maybe Value
depsToJson depTree depInfo rootName = do
  Dep {..} <- depInfo !? rootName
  let childrenNames = findWithDefault [] rootName depTree
  return $ object
    [ ("name"       , toJSON name)
    , ("sha"        , toJSON sha)
    , ("size"       , toJSON size)
    , ("closureSize", toJSON closureSize)
    , ("why"        , toJSON why)
    , ("children", toJSON $ mapM (depsToJson depTree depInfo) childrenNames)
    ]
