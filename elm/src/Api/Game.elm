module Api.Game exposing (..)

import Api.GameState
import Api.GameStatus
import Api.Pair
import Dict
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


type alias Game  =
    { _boardSize : Int
    , _record : List Api.GameState.GameState
    , _komi : Float
    , _finalTerritory : Dict.Dict String (List (Api.Pair.Pair Int))
    , _finalScore : (Float , Float)
    , _status : Api.GameStatus.GameStatus }


encoder : Game -> Json.Encode.Value
encoder a =
    Json.Encode.object [ ("_boardSize" , Json.Encode.int a._boardSize)
    , ("_record" , Json.Encode.list Api.GameState.encoder a._record)
    , ("_komi" , Json.Encode.float a._komi)
    , ("_finalTerritory" , Json.Encode.dict identity (Json.Encode.list (Api.Pair.encoder Json.Encode.int)) a._finalTerritory)
    , ("_finalScore" , case a._finalScore of
        (b , c) ->
            Json.Encode.list identity [ Json.Encode.float b
            , Json.Encode.float c ])
    , ("_status" , Api.GameStatus.encoder a._status) ]


decoder : Json.Decode.Decoder Game
decoder =
    Json.Decode.succeed Game |>
    Json.Decode.Pipeline.required "_boardSize" Json.Decode.int |>
    Json.Decode.Pipeline.required "_record" (Json.Decode.list Api.GameState.decoder) |>
    Json.Decode.Pipeline.required "_komi" Json.Decode.float |>
    Json.Decode.Pipeline.required "_finalTerritory" (Json.Decode.dict (Json.Decode.list (Api.Pair.decoder Json.Decode.int))) |>
    Json.Decode.Pipeline.required "_finalScore" (Json.Decode.map2 Tuple.pair (Json.Decode.index 0 Json.Decode.float) (Json.Decode.index 1 Json.Decode.float)) |>
    Json.Decode.Pipeline.required "_status" Api.GameStatus.decoder
