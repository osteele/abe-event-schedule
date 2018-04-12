module Layout exposing (..)

import List.Extra as List


type alias Block a =
    { event : a, x : Float, width : Float, row : Int, rows : Int }


type alias Row a =
    List (Block a)


makeBlock : a -> Float -> Float -> Block a
makeBlock a x width =
    Block a x width 0 1


overlaps : Block a -> Block a -> Bool
overlaps b1 b2 =
    (b1.x < b2.x + b2.width) && (b2.x < b1.x + b1.width)


{-| Increment the row index of each block by dr for the dr + rowHeight for the second row, ec.
-}
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


{-| If a block could be taller without overlapping another, make it so.
-}
updateBlockHeights : List (Row a) -> List (Row a)
updateBlockHeights rows =
    let
        update rowsBelow block =
            let
                n =
                    List.takeWhile (not << List.any (overlaps block)) rowsBelow
                        |> List.length
            in
                { block | rows = 1 + n }
    in
        case rows of
            [] ->
                []

            r :: rs ->
                (List.map (update rs) r) :: updateBlockHeights rs


{-| Compute a row for each block, and update its {row, rows} fields.
-}
layoutLane : List (Block a) -> List (Block a)
layoutLane events =
    lanes events
        |> adjustRows 1 0
        |> updateBlockHeights
        |> List.concat


{-| Construct a list of rows. Each block is assigned to a row such that it doesn't overlap any other events. New rows are created as necessary.
-}
lanes : List (Block a) -> List (Row a)
lanes events =
    let
        addToRow : Block a -> List (List (Block a)) -> List (List (Block a))
        addToRow event rows =
            case rows of
                [] ->
                    [ [ event ] ]

                r :: rs ->
                    if List.any (\e -> overlaps event e) r then
                        r :: addToRow event rs
                    else
                        (event :: r) :: rs
    in
        List.foldl addToRow [] events
