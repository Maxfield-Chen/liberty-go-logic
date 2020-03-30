module Api.GameState exposing (..)

import Api.Space
import Dict
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


type alias GameState  =
    { _board : Dict.Dict String Api.Space.Space
    , _toPlay : Api.Space.Space
    , _captures : Dict.Dict String Int }


encoder : GameState -> Json.Encode.Value
encoder a =
    Json.Encode.object [ ("_board" , Json.Encode.dict identity Api.Space.encoder a._board)
    , ("_toPlay" , Api.Space.encoder a._toPlay)
    , ("_captures" , Json.Encode.dict identity Json.Encode.int a._captures) ]


decoder : Json.Decode.Decoder GameState
decoder =
    Json.Decode.succeed GameState |>
    Json.Decode.Pipeline.required "_board" (Json.Decode.dict Api.Space.decoder) |>
    Json.Decode.Pipeline.required "_toPlay" Api.Space.decoder |>
    Json.Decode.Pipeline.required "_captures" (Json.Decode.dict Json.Decode.int)
