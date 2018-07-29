module Controls where

import Prelude (type (~>), Unit, Void, bind, const, discard, pure, ($), (<$>), (<>), (==))
import Types (ClosureType(..), UIState)

import D3 (drawSunburst)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML (ClassName(..), HTML, div, form, input, label, span, text)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..), checked, class_, for, id_, name, placeholder, type_, value)
import Network.HTTP.Affjax (get) as AX
import Network.HTTP.Affjax.Response (string) as AX
import SessionStorage as SessionStorage
import Simple.JSON (readJSON, writeJSON)
import Web.Event.Event (preventDefault, Event)

data Query a =
  PreventDefault Event (Query a) |
  PackageValueInput String a |
  ClosureInput ClosureType a |
  RestoreState a |
  Submit a

controls :: H.Component HTML Query Unit Void Aff
controls =
  H.component
    { initialState: const
      { packageName: ""
      , closureType: Runtime
      }
    , render
    , eval
    , receiver: const Nothing
    }
  where
    render :: UIState -> H.ComponentHTML Query
    render state =
      div
        [ class_ $ ClassName "parent" ]
        [ div
          [ class_ $ ClassName "header" ]
          [ form
            [ HE.onSubmit $ \e -> Just $ PreventDefault e (H.action Submit) ]
            [ div
              [ class_ $ ClassName "controls" ]
              [ span
                  [ class_ $ ClassName "name" ]
                  []
              , input
                [ placeholder "Package"
                , type_ InputText
                , value state.packageName
                , HE.onValueInput $ HE.input PackageValueInput
                ]
              , input
                [ type_ InputSubmit
                , value "Go"
                , id_ "submit"
                ]
              , span
                  [ class_ $ ClassName "name" ]
                  []
              , input
                [ type_ InputRadio
                , name "closure"
                , checked (state.closureType == Runtime)
                , HE.onClick $ HE.input_ (ClosureInput Runtime)
                ]
              , label
                [ for "closure" ]
                [ text "Runtime" ]
              , input
                [ type_ InputRadio
                , name "closure"
                , checked (state.closureType == Build)
                , HE.onClick $ HE.input_ (ClosureInput Build)
                ]
              , label
                [ for "closure" ]
                [ text "Build" ]
              ]
            ]
          ]
        , div
          [ id_ "vis" ]
          []
        ]

    eval :: Query ~> H.ComponentDSL UIState Query Void Aff
    eval = case _ of
      PreventDefault event query -> do
        H.liftEffect $ preventDefault event
        eval query
      PackageValueInput value next -> do
        state <- H.get
        H.put $ state { packageName = value }
        pure next
      ClosureInput value next -> do
        state <- H.get
        H.put $ state { closureType = value }
        pure next
      RestoreState next -> do
        item <- H.liftEffect $ SessionStorage.getItem "ui_state"
        case readJSON <$> item of
          Just (Right state) -> do
            H.put state
            eval (Submit next)
          foo -> do
            pure next
      Submit next -> do
        state <- H.get
        H.liftEffect $ SessionStorage.setItem "ui_state" (writeJSON state)
        let
          base =
            case state.closureType of
              Build -> "/build-deps/"
              Runtime -> "/deps/"
          url =
            base <> state.packageName
        res <- H.liftAff $ AX.get AX.string url
        H.liftEffect do
          SessionStorage.setItem "data" (res.response)
          drawSunburst "vis"
        pure next
