module ListTests exposing (listsuite)

import DRec
import Expect exposing (Expectation)
import Json.Encode
import ListTypes
import Test exposing (..)


listsuite : Test
listsuite =
    describe "List tests"
        [ test "setter, getter" <|
            \_ ->
                let
                    values =
                        [ True, False, True ]

                    drec =
                        DRec.setList ListTypes.Booly DRec.fromBool values ListTypes.init

                    res =
                        DRec.get ListTypes.Booly >> DRec.toList DRec.toBool
                in
                Expect.equal (Ok values) (res drec)
        , test "decoding" <|
            \_ ->
                let
                    drec =
                        DRec.decodeString ListTypes.init ListTypes.json
                            |> Result.withDefault ListTypes.init
                in
                Expect.all
                    [ DRec.get ListTypes.Booly >> DRec.toList DRec.toBool >> Expect.equal (Ok [ True, False, True ])
                    , DRec.get ListTypes.Chary >> DRec.toList DRec.toChar >> Expect.equal (Ok [ 'A', 'B', 'C' ])
                    , DRec.get ListTypes.Floaty >> DRec.toList DRec.toFloat >> Expect.equal (Ok [ 1.1, 1.2, 1.3 ])
                    , DRec.get ListTypes.Inty >> DRec.toList DRec.toInt >> Expect.equal (Ok [ 1, 3, 5, 7 ])
                    , DRec.get ListTypes.Jsony >> DRec.toList DRec.toJson >> Expect.equal (Ok [ Json.Encode.string "J", Json.Encode.string "S", Json.Encode.string "O", Json.Encode.string "N" ])
                    , DRec.get ListTypes.Stringy >> DRec.toList DRec.toString >> Expect.equal (Ok [ "lorem", "ipsum" ])
                    ]
                    drec
        , test "encoding" <|
            \_ ->
                let
                    jsonres =
                        DRec.encoder ListTypes.values
                            |> Json.Encode.encode 0
                in
                Expect.equal ListTypes.json jsonres
        ]
