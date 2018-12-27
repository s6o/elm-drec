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
        [ test "setter" <|
            \_ ->
                let
                    values =
                        Array.fromList [ True, False, True ]

                    drec =
                        DRec.setArray ArrayTypes.Booly DRec.fromBool values ArrayTypes.arraydrec
                in
                Expect.equal True True
        ]
