module Api.Group exposing (..)

import Api.Pair
import Api.Space
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


type alias Group  =
    { _liberties : List (Api.Pair.Pair Int)
    , _members : List (Api.Pair.Pair Int)
    , _player : Api.Space.Space }


encoder : Group -> Json.Encode.Value
encoder a =
    Json.Encode.object [ ("_liberties" , Json.Encode.list (Api.Pair.encoder Json.Encode.int) a._liberties)
    , ("_members" , Json.Encode.list (Api.Pair.encoder Json.Encode.int) a._members)
    , ("_player" , Api.Space.encoder a._player) ]


decoder : Json.Decode.Decoder Group
decoder =
    Json.Decode.succeed Group |>
    Json.Decode.Pipeline.required "_liberties" (Json.Decode.list (Api.Pair.decoder Json.Decode.int)) |>
    Json.Decode.Pipeline.required "_members" (Json.Decode.list (Api.Pair.decoder Json.Decode.int)) |>
    Json.Decode.Pipeline.required "_player" Api.Space.decoder
