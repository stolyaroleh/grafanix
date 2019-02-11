module Controls where

import Affjax (get) as AX
import Affjax.ResponseFormat (string) as AX
import D3 (drawSunburst)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML (ClassName(..), HTML, div, form, input, label, text)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..), checked, class_, for, id_, name, placeholder, type_, value)
import Prelude (type (~>), Unit, Void, bind, const, discard, pure, unit, ($), (<$>), (<>), (==))
import SessionStorage as SessionStorage
import Simple.JSON (readJSON, writeJSON)
import Types (ClosureType(..), SizeFunc(..), UIState)
import Web.Event.Event (preventDefault, Event)

-- Queries change state of a component
data Query a =
  -- Run this query, but first call preventDefault()
  PreventDefault Event (Query a) |
  -- Inputs changed
  PackageValueInput String a |
  ClosureInput ClosureType a |
  SizeFuncInput SizeFunc a |
  -- Page just loaded, read state from session storage and redraw last plot
  RestoreState a |
  -- Refresh plot
  Submit a

controls :: H.Component HTML Query Unit Void Aff
controls =
  H.component
    { initialState: const
      { packageName: ""
      , closureType: Runtime
      , sizeFunc: Count
      }
    , render
    , eval
    , receiver: const Nothing
    }
  where
    -- Given current state, generate some HTML
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
              [ div
                [ class_ $ ClassName "group" ]
                [ input
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
                ]
              , div
                [ class_ $ ClassName "group" ]
                [ text "Closure"
                , div
                  []
                  [ input
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
              , div
                [ class_ $ ClassName "group" ]
                [ text "Arc width"
                , div
                  []
                  [ input
                    [ type_ InputRadio
                    , name "size"
                    , checked (state.sizeFunc == Count)
                    , HE.onClick $ HE.input_ (SizeFuncInput Count)
                    ]
                  , label
                    [ for "size" ]
                    [ text "Subtree size" ]
                  , input
                    [ type_ InputRadio
                    , name "size"
                    , checked (state.sizeFunc == ClosureSize)
                    , HE.onClick $ HE.input_ (SizeFuncInput ClosureSize)
                    ]
                  , label
                    [ for "size" ]
                    [ text "Closure size" ]
                  ]
                ]
              ]
            ]
          ]
        , div
          [ id_ "vis" ]
          []
        ]

    -- Given a query, update state
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
        eval $ Submit next
      SizeFuncInput value next -> do
        state <- H.get
        H.put $ state { sizeFunc = value }
        eval $ Submit next
      RestoreState next -> do
        newState <- H.liftEffect $ SessionStorage.getItem "ui_state"
        case readJSON <$> newState of
          Just (Right state) -> do
            H.put state
            eval (Submit next)
          _ -> do
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
          case res.body of
            Left err -> pure unit
            Right body -> do
              SessionStorage.setItem "data" body
              drawSunburst "vis" (state.sizeFunc == ClosureSize)
        pure next
