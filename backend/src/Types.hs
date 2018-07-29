module Types
  ( App
  , runApp
  , Env(..)
  , Dep(..)
  , Why(..)
  , DepTree(..)
  )
where

import           Control.Error (Script)
import           Data.Aeson (ToJSON, object, toJSON)
import           Data.Tree (Tree(..))
import           Data.LruCache.IO (LruHandle)
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
    -- Cache storing filesystem size lookups.
  , duCache :: LruHandle Text Int
    -- Cache storing reasons why there is a dependency between two store paths (src, dest).
  , whyCache :: LruHandle (Text, Text) [Why]
  }

-- A node in a dependency tree
data Dep = Dep
  { name :: Text
  , sha :: Text
  , size :: Int
  , why :: [Why]
  } deriving (Eq, Show, Generic)

instance ToJSON Dep

-- A reason why a node depends on its parent
data Why = Why
  { file :: Text
  , reason :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON Why

newtype DepTree = DepTree (Tree Dep)
  deriving (Eq, Show)

instance ToJSON DepTree where
  toJSON (DepTree node) = serialize node
    where
      serialize Node {rootLabel=Dep{..}, ..} =
        object
          [ ("name", toJSON name)
          , ("sha", toJSON sha)
          , ("size", toJSON size)
          , ("why", toJSON why)
          , ("children", toJSON $ map serialize subForest)
          ]
