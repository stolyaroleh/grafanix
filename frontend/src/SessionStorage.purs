module SessionStorage
  ( getItem
  , setItem
  , removeItem
  , updateItem
  ) where

import Data.Maybe (Maybe(..))
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)

foreign import getItemImpl :: EffectFn3 (String -> Maybe String) (Maybe String) String (Maybe String)
foreign import setItemImpl :: EffectFn2 String String Unit
foreign import removeItemImpl :: EffectFn1 String Unit

getItem :: String -> Effect (Maybe String)
getItem = runEffectFn3 getItemImpl Just Nothing

setItem :: String -> String -> Effect Unit
setItem = runEffectFn2 setItemImpl

removeItem :: String -> Effect Unit
removeItem = runEffectFn1 removeItemImpl

updateItem :: String -> Maybe String -> Effect Unit
updateItem key =
  case _ of
    Just x -> setItem key x
    Nothing -> removeItem key
