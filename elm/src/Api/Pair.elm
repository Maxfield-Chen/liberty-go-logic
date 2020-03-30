module Api.Pair exposing (..)

import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


type Pair a
    = Pair a a


encoder : (a -> Json.Encode.Value) -> Pair a -> Json.Encode.Value
encoder a b =
    case b of
        Pair c d ->
            Json.Encode.list identity [a c, a d]


decoder : Json.Decode.Decoder a -> Json.Decode.Decoder (Pair a)
decoder a =
    Json.Decode.succeed Pair |>
    Json.Decode.Pipeline.custom (Json.Decode.index 0 a) |>
    Json.Decode.Pipeline.custom (Json.Decode.index 1 a)
