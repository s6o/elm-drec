module Main exposing (Model, Msg(..), initialModel, main, update, view)

import BaseRecord exposing (BaseFields(..), BaseRecord, fieldNames)
import Browser
import Html exposing (Html, button, div, fieldset, h1, h4, input, label, legend, text, textarea)
import Html.Attributes exposing (style, type_, value)


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> initialModel
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = \m -> { title = "elm-drec | form-input-demo", body = [ view m ] }
        }


type alias Model =
    { baseTypes : BaseRecord
    }


type Msg
    = NoOp


initialModel : ( Model, Cmd Msg )
initialModel =
    ( Model BaseRecord.schema
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model
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
            ++ (BaseRecord.fieldNames model.baseTypes
                    |> List.map (\title -> viewEntry title "" Nothing)
               )
        )


viewEntry : String -> String -> Maybe String -> Html Msg
viewEntry title entry merror =
    div
        [ style "margin-bottom" "10px"
        ]
        [ label [] [ text title ]
        , div
            []
            [ input
                [ type_ "text"
                , value entry
                , style "width" "100%"
                ]
                []
            ]
        , div
            [ style "color" "#A00000"
            , style "min-height" "25px"
            ]
            [ merror
                |> Maybe.withDefault ""
                |> text
            ]
        ]
