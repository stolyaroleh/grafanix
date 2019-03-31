module D3 where

import Data.Unit (Unit)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)

foreign import drawGraphImpl :: EffectFn1 String Unit

drawGraph :: String -> Effect Unit
drawGraph = runEffectFn1 drawGraphImpl
