module D3 where

import Data.Unit (Unit)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)

foreign import drawSunburstImpl :: EffectFn2 String Boolean Unit

drawSunburst :: String -> Boolean -> Effect Unit
drawSunburst = runEffectFn2 drawSunburstImpl
