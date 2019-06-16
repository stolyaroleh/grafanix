module Main exposing (main)

import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.Form as Form
import Bootstrap.Form.Fieldset as Fieldset
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Size as Size
import Bootstrap.Utilities.Spacing as Spacing
import Browser
import Html exposing (Html, div, h5, text)
import Html.Attributes exposing (id, style, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import JS exposing (drawGraph, sessionRestore, sessionSave)


type alias Expr =
    String


type alias GraphData =
    String


type ClosureType
    = Build
    | Runtime


type alias Model =
    { expr : String
    , closureType : ClosureType
    , tracker : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { expr = ""
      , closureType = Runtime
      , tracker = Nothing
      }
    , Cmd.none
    )


type Msg
    = SetExpr Expr
    | SetClosureType ClosureType
    | DrawGraph (Result Http.Error GraphData)
    | RestoreSession String
    | Submit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noop =
            ( model, Cmd.none )
    in
    case msg of
        SetExpr newExpr ->
            ( { model | expr = newExpr }, Cmd.none )

        SetClosureType newClosureType ->
            if model.closureType == newClosureType then
                noop

            else
                update Submit { model | closureType = newClosureType }

        DrawGraph data ->
            case data of
                Ok graphData ->
                    ( model, drawGraph graphData )

                Err err ->
                    noop

        RestoreSession newExpr ->
            let
                newModel =
                    { model | expr = newExpr }
            in
            update Submit newModel

        Submit ->
            let
                saveState =
                    sessionSave model.expr
            in
            ( model, Cmd.batch [ saveState, fetchGraph model ] )


fetchGraph : Model -> Cmd Msg
fetchGraph model =
    let
        method =
            case model.closureType of
                Runtime ->
                    "deps/"

                Build ->
                    "build-deps/"

        url =
            "http://localhost:3000/" ++ method ++ model.expr

        maybeCancel tracker =
            case tracker of
                Just s ->
                    [ Http.cancel s ]

                Nothing ->
                    []
    in
    if Just model.expr == model.tracker then
        Cmd.none

    else
        Cmd.batch
            (maybeCancel model.tracker
                ++ [ Http.request
                        { method = "GET"
                        , timeout = Nothing
                        , tracker = Just url
                        , body = Http.emptyBody
                        , headers = []
                        , url = url
                        , expect = Http.expectString DrawGraph
                        }
                   ]
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    sessionRestore RestoreSession


view : Model -> Browser.Document Msg
view model =
    let
        packageInputGroup =
            InputGroup.config
                (InputGroup.text
                    [ Input.placeholder "Package or expression"
                    , Input.onInput SetExpr
                    , Input.attrs [ value model.expr ]
                    ]
                )
                |> InputGroup.attrs [ Size.w100 ]
                |> InputGroup.successors
                    [ InputGroup.button [ Button.primary ] [ text "Go!" ] ]
                |> InputGroup.view

        packageInput =
            Form.form
                [ onSubmit Submit
                , Spacing.pb2
                , Spacing.pt2
                , Spacing.pl3
                , Spacing.pr3
                , Size.w100
                , style "position" "absolute"
                , style "top" "0px"
                ]
                [ packageInputGroup ]

        closureButtonGroup =
            ButtonGroup.radioButtonGroup
                [ ButtonGroup.attrs [ Size.w100 ] ]
                [ ButtonGroup.radioButton
                    (model.closureType == Runtime)
                    [ Button.primary, Button.onClick <| SetClosureType Runtime ]
                    [ text "Runtime" ]
                , ButtonGroup.radioButton
                    (model.closureType == Build)
                    [ Button.primary, Button.onClick <| SetClosureType Build ]
                    [ text "Build" ]
                ]

        closureButtons =
            Fieldset.config
                |> Fieldset.attrs
                    [ Spacing.pb2
                    , Spacing.pt2
                    , Spacing.pl3
                    , Spacing.pr3
                    , style "position" "absolute"
                    , style "bottom" "0px"
                    ]
                |> Fieldset.children
                    [ h5 [] [ text "Closure" ]
                    , closureButtonGroup
                    ]
                |> Fieldset.view
    in
    { title = "Grafanix"
    , body =
        [ div
            [ style "display" "flex"
            , Size.h100
            , Flex.col
            ]
            [ packageInput
            , div [ id "vis" ] []
            , closureButtons
            ]
        ]
    }


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
