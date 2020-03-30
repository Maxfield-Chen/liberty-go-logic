module Api.Outcome exposing (..)

import Json.Decode
import Json.Encode


type Outcome 
    = NoKill 
    | Kill 


encoder : Outcome -> Json.Encode.Value
encoder a =
    case a of
        NoKill ->
            Json.Encode.string "NoKill"
        
        Kill ->
            Json.Encode.string "Kill"


decoder : Json.Decode.Decoder Outcome
decoder =
    Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "NoKill" ->
            Json.Decode.succeed NoKill
        
        "Kill" ->
            Json.Decode.succeed Kill
        
        _ ->
            Json.Decode.fail "No matching constructor")
