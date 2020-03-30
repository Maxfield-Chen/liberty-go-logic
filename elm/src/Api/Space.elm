module Api.Space exposing (..)

import Json.Decode
import Json.Encode


type Space 
    = Black 
    | White 
    | Empty 


encoder : Space -> Json.Encode.Value
encoder a =
    case a of
        Black ->
            Json.Encode.string "Black"
        
        White ->
            Json.Encode.string "White"
        
        Empty ->
            Json.Encode.string "Empty"


decoder : Json.Decode.Decoder Space
decoder =
    Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "Black" ->
            Json.Decode.succeed Black
        
        "White" ->
            Json.Decode.succeed White
        
        "Empty" ->
            Json.Decode.succeed Empty
        
        _ ->
            Json.Decode.fail "No matching constructor")
