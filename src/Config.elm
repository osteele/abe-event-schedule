module Config exposing (..)

import Bitwise
import Array
import Set exposing (Set)
import Char


{-| App and event configuration.

I'm leaving the type signature of this, because it's still changing so often.

-}
config =
    { -- Change this if you fork the repo:
      gitHubRepo = { owner = "osteele", repo = "abvent" }

    -- Dimensional attributes. Some of these need to be changed in concert with
    -- the CSS.
    , laneLabelWidth = 120 -- CSS .swimlane.label.width
    , rowHeight = 40
    , rowPadding = 10
    , hourWidth = 100
    , eventRightMargin = 10

    -- Specific to the event
    , logoPath = "/slacfest.png"

    -- , dataPath = "/events/?start=2018-4-15&end=2018-4-16"
    , startDate = "2018-4-15"
    , endDate = "2018-4-16"
    , rowsPerLane = 3
    , laneNames =
        [ "Entrance", "Upper Level", "Down Stairs", "Work Room" ]
    , colors = Array.fromList [ "#ba263d", "#0090c6", "#705590", "#e37035", "#369249" ]
    }


eventColor : { a | id : String, labels : List String } -> String
eventColor { id, labels } =
    let
        colors =
            config.colors

        defaultColor =
            "#000000"

        n =
            if Set.member "food" <| Set.fromList labels then
                0
            else
                (hash id % (Array.length colors) - 1) + 1
    in
        Array.get n config.colors
            |> Maybe.withDefault defaultColor


collectLocations : List { a | location : Maybe String } -> Set String
collectLocations events =
    events
        |> (List.map .location)
        |> (List.filterMap identity)
        |> Set.fromList


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
