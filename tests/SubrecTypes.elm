module SubrecTypes exposing
    ( Fields(..)
    , init
    , initEntry
    , initPretty
    , initUnit
    , json
    , prettyJson
    , prettyJsonSubs
    , values
    , valuesEntry
    , valuesPretty
    , valuesPrettySubs
    , valuesUnit
    )

import DRec exposing (DRec, DType(..), DValue(..))
import Json.Encode


type Fields
    = Abbr
    | Long
    | Description
    | Measurement
    | Unit
    | Name
    | Entry


initUnit : DRec Fields
initUnit =
    DRec.init
        |> DRec.field Abbr DString
        |> DRec.field Long DString


initUnitPrettySubs : DRec Fields
initUnitPrettySubs =
    DRec.initWithIndent 3
        |> DRec.field Abbr DString
        |> DRec.field Long DString


initEntry : DRec Fields
initEntry =
    DRec.init
        |> DRec.field Description DString
        |> DRec.field Measurement DFloat
        |> DRec.field Unit (DDRec <| DRec.schema initUnit)


initEntryPrettySubs : DRec Fields
initEntryPrettySubs =
    DRec.initWithIndent 3
        |> DRec.field Description DString
        |> DRec.field Measurement DFloat
        |> DRec.field Unit (DDRec <| DRec.schema initUnitPrettySubs)


init : DRec Fields
init =
    DRec.init
        |> DRec.field Name DString
        |> DRec.field Entry (DDRec <| DRec.schema initEntry)


initPretty : DRec Fields
initPretty =
    DRec.initWithIndent 4
        |> DRec.field Name DString
        |> DRec.field Entry (DDRec <| DRec.schema initEntry)


initPrettySubs : DRec Fields
initPrettySubs =
    DRec.initWithIndent 4
        |> DRec.field Name DString
        |> DRec.field Entry (DDRec <| DRec.schema initEntryPrettySubs)


valuesUnit : DRec Fields
valuesUnit =
    initUnit
        |> DRec.setString Abbr "m"
        |> DRec.setString Long "meters"


valuesUnitPrettySubs : DRec Fields
valuesUnitPrettySubs =
    initUnitPrettySubs
        |> DRec.setString Abbr "m"
        |> DRec.setString Long "meters"


valuesEntry : DRec Fields
valuesEntry =
    initEntry
        |> DRec.setString Description "Tower measurement"
        |> DRec.setFloat Measurement 314.5
        |> DRec.setDRec Unit valuesUnit


valuesEntryPrettySubs : DRec Fields
valuesEntryPrettySubs =
    initEntryPrettySubs
        |> DRec.setString Description "Tower measurement"
        |> DRec.setFloat Measurement 314.5
        |> DRec.setDRec Unit valuesUnitPrettySubs


values : DRec Fields
values =
    init
        |> DRec.setString Name "Level 0"
        |> DRec.setDRec Entry valuesEntry


valuesPretty : DRec Fields
valuesPretty =
    initPretty
        |> DRec.setString Name "Level 0"
        |> DRec.setDRec Entry valuesEntry


valuesPrettySubs : DRec Fields
valuesPrettySubs =
    initPrettySubs
        |> DRec.setString Name "Level 0"
        |> DRec.setDRec Entry valuesEntryPrettySubs


json : String
json =
    """{"name":"Level 0","entry":{"description":"Tower measurement","measurement":314.5,"unit":{"abbr":"m","long":"meters"}}}"""


prettyJson : String
prettyJson =
    """{
    "name": "Level 0",
    "entry": {
        "description": "Tower measurement",
        "measurement": 314.5,
        "unit": {
            "abbr": "m",
            "long": "meters"
        }
    }
}"""


prettyJsonSubs : String
prettyJsonSubs =
    """{
    "name": "Level 0",
    "entry": {
        "description": "Tower measurement",
        "measurement": 314.5,
        "unit": {
            "abbr": "m",
            "long": "meters"
        }
    }
}"""
