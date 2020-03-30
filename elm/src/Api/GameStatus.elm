module Api.GameStatus exposing (..)

import Json.Decode
import Json.Encode


type GameStatus 
    = GameRejected 
    | GameProposed 
    | InProgress 
    | CountingProposed 
    | CountingAccepted 
    | TerritoryProposed 
    | TerritoryAccepted 


encoder : GameStatus -> Json.Encode.Value
encoder a =
    case a of
        GameRejected ->
            Json.Encode.string "GameRejected"
        
        GameProposed ->
            Json.Encode.string "GameProposed"
        
        InProgress ->
            Json.Encode.string "InProgress"
        
        CountingProposed ->
            Json.Encode.string "CountingProposed"
        
        CountingAccepted ->
            Json.Encode.string "CountingAccepted"
        
        TerritoryProposed ->
            Json.Encode.string "TerritoryProposed"
        
        TerritoryAccepted ->
            Json.Encode.string "TerritoryAccepted"


decoder : Json.Decode.Decoder GameStatus
decoder =
    Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "GameRejected" ->
            Json.Decode.succeed GameRejected
        
        "GameProposed" ->
            Json.Decode.succeed GameProposed
        
        "InProgress" ->
            Json.Decode.succeed InProgress
        
        "CountingProposed" ->
            Json.Decode.succeed CountingProposed
        
        "CountingAccepted" ->
            Json.Decode.succeed CountingAccepted
        
        "TerritoryProposed" ->
            Json.Decode.succeed TerritoryProposed
        
        "TerritoryAccepted" ->
            Json.Decode.succeed TerritoryAccepted
        
        _ ->
            Json.Decode.fail "No matching constructor")
