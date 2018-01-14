# elm-drec

Elm `Dict` based record with field name and type validation and automatic
decoding from and to JSON.

When you control your backend (e.g. PostgREST <https://github.com/begriffs/postgrest>)
or work with well formatted JSON sources, writing decoders and encoders,
quickly becomes rather repetitive.

The elm-drec library provides an alternative record type `DRec` which can provide
decoding and encoding based on `DRec`'s schema.

## Features / Trade-offs
  * schema based runtime field name and type validation
  * automatic schema based decoding and encoding from and to JSON
  * user input buffering (partial input validation failure handling)
  * extensible records (new fields can be added if data is present)
  * No reliance on `Debug.crash`, `Result x a` is used instead
  * `Dict comparable v` and `Set comparable` member support excluded,
    due to sum types not being `comparable` (at present an impl. limitation)
  * Custom types are supported via serialization to/from JSON (`Json.Encode.Value`)

### Supported types
  * Array (Bool, DRec, Float, Int, Json.Encode.Value, String)
  * Bool
  * DRec (nesting / sub-records)
  * Float
  * Int
  * Json.Encode.Value
  * List (Bool, DRec, Float, Int, Json.Encode.Value, String)
  * Maybe (Bool, DRec, Float, Int, Json.Encode.Value, String)
  * String

## Usage examples

### Schema
```
Elm record                                  elm-drec

import Array exposing (Array)               import Dict exposing (Dict)
import Dict exposing (Dict)                 import DRec
                                                exposing
                                                    ( DError
                                                    , DRec
                                                    , DType(..)
                                                    , DValue(..)
                                                    , init, field, schema)

type alias Address =                        address : DRec
    { streetName : String                   address =
    , buildingNumer : Int                       init
    , subNumber : Maybe Int                         |> field "street_name" DString
    , deliveryDays : Array Int                      |> field "building_number" DInt
    }                                               |> field "sub_number" (DMaybe VInt)
                                                    |> field "delivery_days" (DArray VInt)

type alias Customer =                       customer : DRec
    { id : Int                              customer =
    , name : String                             init
    , address : Address                             |> field "id" DInt
    }                                               |> field "name" DString
                                                    |> field "address" (DDRec <| schema address)

type alias Model =                          type alias Model =
    { customers : Dict Int Customer             { customers : Dict Int DRec
    }                                           }
```

### Decoding
```
Elm                                         elm-drec

import Json.Decode                          import DRec
    exposing
        ( Decoder, field
        , map3, map4
        , array, int, maybe, string)

addressDecoder : Decoder Address            DRec.decoder address
addressDecoder =
    map4 Address
        (field "street_name" string)
        (field "building_number" int)
        (maybe (field "sub_number" int))
        (field "delivery_days" <| array int)

customerDecoder : Decoder Customer          DRec.decoder customer
customerDecoder =
    map3 Customer
        (field "id" int)
        (field "name" string)
        (field "address" addressDecoder)

init : Model
init =
    let
        json =
            """
            {"id":1
            ,"name":"John Doe"
            ,"address":{"street_name":"Lost","building_number":42,"delivery_days":[3,7,13,21]}
            }
            """
    in
    { model
        | customers =
            Json.Decode.decodeString                    DRec.decodeString
                customerDecoder                             customer
                json                                        json
                    |> Result.map                               |> Result.map
                        (\c ->                                      (\drec ->
                            Dict.insert                                 DRec.get "id" drec
                                c.id                                        |> DRec.toInt
                                c                                           |> Result.map (\i -> Dict.insert i drec Dict.empty)
                                Dict.empty                                  |> Result.withDefault Dict.empty
                        )                                           )
                    |> Result.withDefault Dict.empty            |> Result.withDefault Dict.empty
    }
```

### Encoding
```
Elm                                         elm-drec

import Array
import Json.Encode                          import DRec
    exposing
        ( array
        , int
        , null
        , object
        , string
        )

encoderA : Address -> Json.Encode.Value
encoderA a =
    object
        [ ( "street_name"
          , string c.streetName
          )
        , ( "building_number"
          , int c.buildingNumber
          )
        , ( "sub_number"
          , c.subNumber
                |> Maybe.map int
                |> Maybe.withDefault null
          )
        , ( "deliver_days",
          , c.deliveryDays
                |> Array.map int
                |> array
          )
        ]

encoderC : Customer -> Json.Encode.Value
encoder c =
    object
        [ ( "id", int c.id )
        , ( "name", string c.name )
        , ( "address", encoderA c.address )
        ]

stringify : Int -> Model -> String
stringify id model =
    model.customers
        |> Dict.get id
        |> Maybe.map
            (\c ->                              (\drec ->
                encoderC c                          DRec.encoder drec
                    |> Json.Encode.encode 0             |> Json.Encode.encode 0
            )                                   )
```
