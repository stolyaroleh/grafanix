module Main where

import Controls (Query(..), controls)
import Effect (Effect)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Prelude (Unit, bind, unit, ($))

main :: Effect Unit
main =
  runHalogenAff do
    body <- awaitBody
    io <- runUI controls unit body
    io.query $ H.action RestoreState
