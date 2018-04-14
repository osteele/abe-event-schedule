module HelpersTests exposing (..)

import Helpers exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "helpers"
        [ describe "intersects" <|
            [ -- These tests don't cover the case where enpoints exactly coincide.
              -- For the application at hand we don't really care.
              test "a entirely left of b" <|
                \_ ->
                    intersects ( 10, 20 ) ( 30, 50 )
                        |> Expect.equal False
            , test "a right intersects b left" <|
                \_ ->
                    intersects ( 10, 40 ) ( 30, 50 )
                        |> Expect.equal True
            , test "a within b" <|
                \_ ->
                    intersects ( 35, 45 ) ( 30, 50 )
                        |> Expect.equal True
            , test "a left intersects b right" <|
                \_ ->
                    intersects ( 40, 60 ) ( 30, 50 )
                        |> Expect.equal True
            , test "a entirely right of b" <|
                \_ ->
                    intersects ( 50, 60 ) ( 30, 40 )
                        |> Expect.equal False
            ]
        , test "scaleAround" <|
            \_ ->
                scaleAround 100 2 150
                    |> Expect.equal 200
        ]
