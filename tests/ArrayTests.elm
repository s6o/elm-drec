module ArrayTests exposing (arraysuite)

import Array
import ArrayTypes
import DRec
import Expect exposing (Expectation)
import Json.Encode
import Test exposing (..)


arraysuite : Test
arraysuite =
    describe "Array tests"
        [ test "setter, getter" <|
            \_ ->
                let
                    values =
                        Array.fromList [ True, False, True ]

                    drec =
                        DRec.setArray ArrayTypes.Booly DRec.fromBool values ArrayTypes.init

                    res =
                        DRec.get ArrayTypes.Booly >> DRec.toArray DRec.toBool
                in
                Expect.equal (Ok values) (res drec)
        , test "decoding" <|
            \_ ->
                let
                    drec =
                        DRec.decodeString ArrayTypes.init ArrayTypes.json
                            |> Result.withDefault ArrayTypes.init
                in
                Expect.all
                    [ DRec.get ArrayTypes.Booly >> DRec.toArray DRec.toBool >> Expect.equal (Ok <| Array.fromList [ True, False, True ])
                    , DRec.get ArrayTypes.Chary >> DRec.toArray DRec.toChar >> Expect.equal (Ok <| Array.fromList [ 'A', 'B', 'C' ])
                    , DRec.get ArrayTypes.Floaty >> DRec.toArray DRec.toFloat >> Expect.equal (Ok <| Array.fromList [ 1.1, 1.2, 1.3 ])
                    , DRec.get ArrayTypes.Inty >> DRec.toArray DRec.toInt >> Expect.equal (Ok <| Array.fromList [ 1, 3, 5, 7 ])
                    , DRec.get ArrayTypes.Jsony >> DRec.toArray DRec.toJson >> Expect.equal (Ok <| Array.fromList [ Json.Encode.string "J", Json.Encode.string "S", Json.Encode.string "O", Json.Encode.string "N" ])
                    , DRec.get ArrayTypes.Stringy >> DRec.toArray DRec.toString >> Expect.equal (Ok <| Array.fromList [ "lorem", "ipsum" ])
                    ]
                    drec
        , test "encoding" <|
            \_ ->
                let
                    jsonres =
                        DRec.encoder ArrayTypes.values
                            |> Json.Encode.encode 0
                in
                Expect.equal ArrayTypes.json jsonres
        ]
