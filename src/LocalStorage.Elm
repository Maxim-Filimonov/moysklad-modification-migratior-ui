port module LocalStorage exposing (Event(..), lookup, put, subscription)

import Json.Decode as Decode
import Json.Encode as Encode


type Event
    = ErrorReading String
    | GotJwtToken String


lookup : String -> Cmd msg
lookup key =
    Encode.string "jwtToken"
        |> localStorageToJs


put : String -> Cmd msg
put value =
    Encode.object
        [ ( "key", Encode.string "jwt-token" )
        , ( "value", Encode.string value )
        ]
        |> localStorageToJs


subscription : Sub Event
subscription =
    localStorageFromJs convertToEvent


convertToEvent : Decode.Value -> Event
convertToEvent value =
    Json.Decode.succeed GotJwtToken <|
        Decode.string value


port localStorageFromJs : (Decode.Value -> msg) -> Sub msg


port localStorageToJs : Encode.Value -> Cmd msg
