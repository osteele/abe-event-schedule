module Config exposing (..)

import Bitwise
import Array
import Set
import Char


config =
    { laneLabelWidth = 120
    , rowHeight = 40
    , rowPadding = 10
    , hourWidth = 100
    , xMargin = 10
    , logoPath = "/slacfest.png"
    , dataPath = "/events/?start=2018-4-15&end=2018-4-16"
    , laneNames =
        [ "Entrance", "Upper Level", "Down Stairs", "Work Room" ]
    , colors = Array.fromList [ "#ba263d", "#0090c6", "#705590", "#e37035", "#369249" ]
    }


eventColor : { a | id : String, labels : List String } -> String
eventColor { id, labels } =
    let
        colors =
            config.colors

        n =
            if Set.member "food" <| Set.fromList labels then
                0
            else
                (hash id % ((Array.length colors) - 1)) + 1
    in
        Array.get n config.colors
            |> Maybe.withDefault "#000000"



-- swimlanes : Result String (List String)
-- swimlanes =
--     events
--         |> Result.map (List.map .location)
--         |> Result.map (List.filterMap identity)
--         |> Result.map Set.fromList
--         |> Result.map Set.toList


{-| Translated from <http://www.cse.yorku.ca/~oz/hash.html#djb2>.
-}
hash : String -> Int
hash str =
    let
        updateHash c h =
            (h * 33)
                + Char.toCode c
                |> Bitwise.and 65535
    in
        String.foldl updateHash 5381 str
