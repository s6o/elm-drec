module User exposing (UserField(..), fieldNames, init, johndoe)

import DRec exposing (DRec, DType(..), DValue(..))


type UserField
    = Id
    | Email
    | Name
    | Token


init : DRec UserField
init =
    DRec.init
        |> DRec.field Id DInt
        |> DRec.field Email DString
        |> DRec.field Name DString
        |> DRec.field Token (DMaybe VString)


fieldNames : List UserField
fieldNames =
    [ Id
    , Email
    , Name
    , Token
    ]


johndoe : String
johndoe =
    """
        {"id":1
        ,"email":"john.doe@lost.net"
        ,"name":"John Doe"
        ,"token":"abcdef01234"
        }
    """
