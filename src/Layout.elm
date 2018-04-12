module Layout exposing (..)


type alias Block a =
    { event : a, x : Float, width : Float, row : Int }


type alias Row a =
    List (Block a)


makeBlock : a -> Float -> Float -> Block a
makeBlock a x width =
    Block a x width 0


adjustRows : Int -> Int -> List (Row a) -> List (Row a)
adjustRows rowHeight dr rows =
    case rows of
        [] ->
            []

        r :: rs ->
            let
                newRow =
                    List.map (\e -> { e | row = e.row + dr }) r
            in
                newRow :: adjustRows rowHeight (dr + rowHeight) rs


layoutLane : List (Block a) -> List (Block a)
layoutLane events =
    lanes events
        |> adjustRows 1 0
        |> List.concat


lanes : List (Block a) -> List (List (Block a))
lanes events =
    let
        addToRow : Block a -> List (List (Block a)) -> List (List (Block a))
        addToRow event rows =
            case rows of
                [] ->
                    [ [ event ] ]

                r :: rs ->
                    if List.any (\e -> eventsOverlap event e) r then
                        r :: addToRow event rs
                    else
                        (event :: r) :: rs

        eventsOverlap : Block a -> Block a -> Bool
        eventsOverlap b1 b2 =
            (b1.x < b2.x + b2.width) && (b2.x < b1.x + b1.width)
    in
        List.foldl addToRow [] events
