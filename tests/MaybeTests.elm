module MaybeTests exposing (maybesuite)

import DRec
import Expect exposing (Expectation)
import Json.Encode
import MaybeTypes
import Test exposing (..)


maybesuite : Test
maybesuite =
    describe "Maybe tests"
        [ test "setter, getter" <|
            \_ ->
                let
                    drec =
                        DRec.setMaybe MaybeTypes.Booly DRec.fromBool (Just False) MaybeTypes.init

                    res =
                        DRec.get MaybeTypes.Booly >> DRec.toMaybe DRec.toBool
                in
                Expect.equal (Ok (Just False)) (res drec)
        , test "decoding" <|
            \_ ->
                let
                    drec =
                        DRec.decodeString MaybeTypes.init MaybeTypes.json
                            |> Result.withDefault MaybeTypes.init
                in
                Expect.all
                    [ DRec.get MaybeTypes.Booly >> DRec.toMaybe DRec.toBool >> Expect.equal (Ok (Just True))
                    , DRec.get MaybeTypes.Chary >> DRec.toMaybe DRec.toChar >> Expect.equal (Ok (Just 'A'))
                    , DRec.get MaybeTypes.Floaty >> DRec.toMaybe DRec.toFloat >> Expect.equal (Ok (Just 1.2))
                    , DRec.get MaybeTypes.Inty >> DRec.toMaybe DRec.toInt >> Expect.equal (Ok (Just 3))
                    , DRec.get MaybeTypes.Jsony >> DRec.toMaybe DRec.toJson >> Expect.equal (Ok (Just (Json.Encode.string "J")))
                    , DRec.get MaybeTypes.Stringy >> DRec.toMaybe DRec.toString >> Expect.equal (Ok (Just "lorem ipsum"))
                    ]
                    drec
        , test "encoding" <|
            \_ ->
                let
                    jsonres =
                        DRec.encode MaybeTypes.values
                            |> Json.Encode.encode 0
                in
                Expect.equal MaybeTypes.json jsonres
        , test "encoding, Nothing values" <|
            \_ ->
                let
                    jsonres =
                        DRec.encode MaybeTypes.novalues
                            |> Json.Encode.encode 0
                in
                Expect.equal MaybeTypes.nojson jsonres
        ]
