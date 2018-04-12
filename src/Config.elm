module Config exposing (..)


config =
    { rowHeight = 40
    , rowPadding = 10
    , hourWidth = 100
    , dataUrl = "http://localhost:3000/events/?start=2018-4-15&end=2018-4-16"
    , swimlaneNames =
        [ "Entrance", "Upper Level", "Down Stairs", "Work Room" ]
    }



-- swimlanes : Result String (List String)
-- swimlanes =
--     events
--         |> Result.map (List.map .location)
--         |> Result.map (List.filterMap identity)
--         |> Result.map Set.fromList
--         |> Result.map Set.toList
