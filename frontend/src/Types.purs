module Types where

import Prelude

import Foreign (ForeignError(..), fail, readString)
import Simple.JSON (class ReadForeign, class WriteForeign, writeImpl)

data ClosureType = Build | Runtime

derive instance eCT :: Eq ClosureType

instance sCT :: Show ClosureType where
  show Build = "Build"
  show Runtime = "Runtime"

instance rfCT :: ReadForeign ClosureType where
  readImpl f = do
    s <- readString f
    case s of
      "Build" -> pure Build
      "Runtime" -> pure Runtime
      _ -> fail $ ForeignError "Can't read ClosureType"

instance wfCT :: WriteForeign ClosureType where
  writeImpl = writeImpl <<< show

type UIState =
  { packageName :: String
  , closureType :: ClosureType
  }
