module D3 where

import Data.Unit (Unit)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)

foreign import drawSunburstImpl :: EffectFn1 String Unit

drawSunburst :: String -> Effect Unit
drawSunburst = runEffectFn1 drawSunburstImpl
