module DecoderExtra exposing (..)

import Date exposing (Date)
import Json.Decode exposing (..)
import Json.Decode.Pipeline as Pipeline


dateDecoder : Decoder Date
dateDecoder =
    let
        toIsoDate =
            String.split " " >> String.join "T"
    in
        customDecoder string (Date.fromString << toIsoDate)


customDecoder : Decoder a -> (a -> Result String b) -> Decoder b
customDecoder decoder parser =
    decoder
        |> andThen (resultToDecoder << parser)


optional : String -> Decoder a -> Decoder (Maybe a -> b) -> Decoder b
optional field decoder =
    Pipeline.optional field (nullable decoder) Nothing


resultToDecoder : Result String a -> Decoder a
resultToDecoder result =
    case result of
        Err e ->
            fail e

        Ok a ->
            succeed a
