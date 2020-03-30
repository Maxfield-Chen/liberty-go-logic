module Api.MoveError exposing (..)

import Json.Decode
import Json.Encode


type MoveError 
    = IllegalPlayer 
    | NoBoard 
    | IllegalKo 
    | Suicide 
    | OutOfBounds 
    | Occupied 


encoder : MoveError -> Json.Encode.Value
encoder a =
    case a of
        IllegalPlayer ->
            Json.Encode.string "IllegalPlayer"
        
        NoBoard ->
            Json.Encode.string "NoBoard"
        
        IllegalKo ->
            Json.Encode.string "IllegalKo"
        
        Suicide ->
            Json.Encode.string "Suicide"
        
        OutOfBounds ->
            Json.Encode.string "OutOfBounds"
        
        Occupied ->
            Json.Encode.string "Occupied"


decoder : Json.Decode.Decoder MoveError
decoder =
    Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "IllegalPlayer" ->
            Json.Decode.succeed IllegalPlayer
        
        "NoBoard" ->
            Json.Decode.succeed NoBoard
        
        "IllegalKo" ->
            Json.Decode.succeed IllegalKo
        
        "Suicide" ->
            Json.Decode.succeed Suicide
        
        "OutOfBounds" ->
            Json.Decode.succeed OutOfBounds
        
        "Occupied" ->
            Json.Decode.succeed Occupied
        
        _ ->
            Json.Decode.fail "No matching constructor")
