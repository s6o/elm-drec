module Main exposing (Model, Msg(..), initialModel, main, update, view)

import BaseRecord exposing (BaseFields(..), BaseRecord, fieldNames)
import Browser
import DRec exposing (DError(..))
import Html exposing (Html, button, div, fieldset, h1, h4, input, label, legend, text, textarea)
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (onInput)


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> initialModel
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = \m -> { title = "elm-drec | form-input-demo", body = [ view m ] }
        }


type alias Model =
    { baseRecord : BaseRecord
    }


type Msg
    = NoOp
    | Text BaseFields String


initialModel : ( Model, Cmd Msg )
initialModel =
    ( Model BaseRecord.schema
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )

        Text bf str ->
            ( { model | baseRecord = DRec.update bf str model.baseRecord }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div
        [ style "font-family" "Monospace" ]
        [ h1 [] [ text "elm-drec | form-input-demo" ]
        , div
            [ style "display" "flex"
            ]
            [ div
                [ style "flex" "60%"
                ]
                [ div
                    [ style "display" "flex"
                    ]
                    [ div
                        [ style "flex" "35%"
                        ]
                        [ viewBaseTypes model
                        ]
                    , div
                        [ style "flex" "65%"
                        ]
                        [ textarea
                            [ style "width" "100%"
                            , style "height" "100%"
                            ]
                            []
                        , div
                            []
                            [ button [] [ h4 [] [ text "Decode, to disply in form" ] ]
                            ]
                        ]
                    ]
                ]
            , div
                [ style "flex" "40%"
                ]
                []
            ]
        ]


viewBaseTypes : Model -> Html Msg
viewBaseTypes model =
    fieldset
        []
        ([ legend [] [ text "Base Types" ]
         ]
            ++ (BaseRecord.fieldNames model.baseRecord
                    |> List.map (\field -> viewEntry field (DRec.retrieve field model.baseRecord))
               )
        )


viewEntry : BaseFields -> ( String, Maybe DError ) -> Html Msg
viewEntry field ( entry, merror ) =
    div
        [ style "margin-bottom" "10px"
        ]
        [ label [] [ text <| BaseRecord.fieldLabel field ]
        , div
            []
            [ input
                [ type_ "text"
                , value entry
                , style "width" "100%"
                , onInput (Text field)
                ]
                []
            ]
        , div
            [ style "color" "#A00000"
            , style "min-height" "25px"
            ]
            [ merror
                |> DRec.isInvalid
                |> Maybe.withDefault ""
                |> text
            ]
        ]
