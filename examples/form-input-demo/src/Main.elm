module Main exposing (Model, Msg(..), initialModel, main, update, view)

import BaseRecord exposing (BaseFields(..), BaseRecord, fieldNames)
import Browser
import DRec exposing (DError(..))
import Html exposing (Html, button, div, fieldset, h1, h4, input, label, legend, text, textarea)
import Html.Attributes exposing (checked, style, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)


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
    , baseJson : String
    , decodeError : Maybe DError
    }


type Msg
    = NoOp
    | FieldInput BaseFields String
    | JsonInput String
    | Encode
    | Decode


initialModel : ( Model, Cmd Msg )
initialModel =
    ( { baseRecord = BaseRecord.initialValues
      , baseJson = DRec.stringify BaseRecord.initialValues
      , decodeError = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )

        FieldInput bf str ->
            ( { model | baseRecord = DRec.update bf str model.baseRecord }
            , Cmd.none
            )

        JsonInput str ->
            ( { model | baseJson = str }
            , Cmd.none
            )

        Encode ->
            ( { model | baseJson = DRec.stringify model.baseRecord }
            , Cmd.none
            )

        Decode ->
            let
                decres =
                    DRec.decodeString BaseRecord.schema model.baseJson
            in
            ( { model
                | baseRecord = decres |> Result.withDefault model.baseRecord
                , decodeError =
                    case decres of
                        Err derr ->
                            Just derr

                        _ ->
                            Nothing
              }
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
                        , button
                            [ onClick Encode ]
                            [ h4 [] [ text "Encode" ]
                            ]
                        ]
                    , div
                        [ style "flex" "65%"
                        ]
                        [ textarea
                            [ style "width" "100%"
                            , style "height" "100%"
                            , style "font-size" "14pt"
                            , onInput JsonInput
                            ]
                            [ text model.baseJson
                            ]
                        , div
                            []
                            [ button
                                [ onClick Decode ]
                                [ h4 [] [ text "Decode, to disply in form" ]
                                ]
                            ]
                        , div
                            [ style "color" "#A00000"
                            , style "min-height" "25px"
                            ]
                            [ model.decodeError
                                |> DRec.isDecodingFailure
                                |> Maybe.withDefault ""
                                |> text
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
            [ case field of
                Checky ->
                    input
                        [ type_ "checkbox"
                        , checked <| Maybe.withDefault False <| DRec.asBool entry
                        , onCheck (Debug.toString >> FieldInput field)
                        ]
                        []

                _ ->
                    input
                        [ type_ "text"
                        , value entry
                        , style "width" "100%"
                        , onInput (FieldInput field)
                        ]
                        []
            ]
        , div
            [ style "color" "#A00000"
            , style "min-height" "25px"
            ]
            [ (if DRec.isInvalid merror then
                "Incorrect value"

               else
                ""
              )
                |> text
            ]
        ]
