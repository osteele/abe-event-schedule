module Layout
    exposing
        ( Block
        , Row
        , layoutLane
        , makeBlock
        )

{-| Functions to lay out lists of blocks into rows..
-}

import Helpers exposing (intersects)
import List.Extra as List


{-| The layout algorithms accept and returns lists of structures of this type.
They read the `left` and `right` fields, and set `row` and `rows`.
-}
type alias Block model =
    { model : model -- the underlying model
    , left : Float
    , right : Float
    , row : Int -- the assigned row
    , rows : Int -- the number of rows high
    }


{-| A `Row` is a list of `Block`s. This type simplifies some of the function
type signatures below. Not every `List Block` is typed as `Row`: some `List
Block`s represent just that — a list of `Block`, that hasn't been organized into
a row of same.
-}
type alias Row a =
    List (Block a)


{-| Make a `Block`.
-}
makeBlock : model -> Float -> Float -> Block model
makeBlock model x width =
    Block model x (x + width) 0 1


{-| Indicate whether two blocks overlap in x.
-}
overlapsInX : Block a -> Block a -> Bool
overlapsInX a b =
    intersects ( a.left, a.right ) ( b.left, b.right )


{-| Increment the block.row fields of each row's blocks by rowHeight times
the number of preceding rows.
-}
incrementRowIndexes : Int -> List (Row a) -> List (Row a)
incrementRowIndexes rowHeight rows =
    let
        adjust accum rows =
            case rows of
                [] ->
                    []

                r :: rs ->
                    let
                        newRow =
                            List.map (\e -> { e | row = e.row + accum }) r
                    in
                        newRow :: adjust (accum + rowHeight) rs
    in
        adjust 0 rows


{-| Adjust the heights (`rows` fields) of the blocks. Increase each block's rows
as to cover following rows if this doesn't cause a collision.
-}
updateBlockHeights : List (Row a) -> List (Row a)
updateBlockHeights rows =
    let
        update rowsBelow block =
            let
                n =
                    List.takeWhile (not << List.any (overlapsInX block)) rowsBelow
                        |> List.length
            in
                { block | rows = 1 + n }
    in
        case rows of
            [] ->
                []

            r :: rs ->
                List.map (update rs) r :: updateBlockHeights rs


{-| Compute a row for each block, and update its {row, rows} fields.
-}
layoutLane : List (Block a) -> List (Block a)
layoutLane events =
    lanes events
        |> incrementRowIndexes 1
        |> updateBlockHeights
        |> List.concat


{-| Construct a list of rows. Each block is assigned to a row such that it
doesn't overlap any other events. New rows are created as necessary.
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
                    if List.any (\e -> overlapsInX event e) r then
                        r :: addToRow event rs
                    else
                        (event :: r) :: rs
    in
        List.foldl addToRow [] events
