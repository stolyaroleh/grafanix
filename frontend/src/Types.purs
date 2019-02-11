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

data SizeFunc = Count | ClosureSize

derive instance eSF :: Eq SizeFunc

instance sSF :: Show SizeFunc where
  show Count = "Count"
  show ClosureSize = "ClosureSize"

instance rfSF :: ReadForeign SizeFunc where
  readImpl f = do
    s <- readString f
    case s of
      "Count" -> pure Count
      "ClosureSize" -> pure ClosureSize
      _ -> fail $ ForeignError "Can't read SizeFunc"

instance wfSF :: WriteForeign SizeFunc where
  writeImpl = writeImpl <<< show

type UIState =
  { packageName :: String
  , closureType :: ClosureType
  , sizeFunc :: SizeFunc
  }
