module DecoderExtra
    exposing
        ( dateDecoder
        , optional
        )

{-| Extra functions for working with Json.Decode.

These functions aren't specific to this application or domain.

-}

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


{-| Make a Decoder from a parser. Adapted krisajenkins/elm-exts.
-}
customDecoder : Decoder a -> (a -> Result String b) -> Decoder b
customDecoder decoder parser =
    decoder
        |> andThen (resultToDecoder << parser)


{-| Decode a field that may be missing or have a null value. If the field is
missing or null, then it decodes as `Nothing`. If the field is present,
then `decoder` is used to decode its value, which is wrapped in `Just`.

N.b. This differs from `Json.Decode.Pipeline.optional`.

-}
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
