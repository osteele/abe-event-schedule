module Helpers exposing (..)

import Task


intersects : ( Float, Float ) -> ( Float, Float ) -> Bool
intersects a b =
    let
        ( a0, a1 ) =
            a

        ( b0, b1 ) =
            b

        between x ( left, right ) =
            left <= x && x < right
    in
        between a0 b || between b0 a


{-| Scale x by scale around origin.
-}
scaleAround : Float -> Float -> Float -> Float
scaleAround origin scale x =
    x
        |> flip (-) origin
        |> (*) scale
        |> (+) origin


{-| Send a message.
-}
send : a -> Cmd a
send =
    Task.succeed >> Task.perform identity


{-| Poor-person's URL join. This just handles the case of joining a base with
a path, with all possible combinations of '/' and the end of the base and '/'
at the start of the path. It assumes the path is meant to be absolute, and
doesn't handle `..`.
-}
urlJoin : String -> String -> String
urlJoin base path =
    case ( String.endsWith "/" base, String.startsWith "/" path ) of
        ( True, True ) ->
            base ++ String.dropLeft 1 path

        ( False, False ) ->
            base ++ "/" ++ path

        _ ->
            base ++ path
