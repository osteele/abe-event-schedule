module Main exposing (..)

import Css
import Data exposing (json)
import Date exposing (Date)
import DecoderExtra exposing (..)
import Html
import Html.Styled exposing (..)
import Json.Decode
import Json.Decode.Pipeline as Pipeline exposing (decode, required)
import Html.Styled.Attributes exposing (class, css)


main : Program Never Model msg
main =
    Html.beginnerProgram
        { view = view >> toUnstyled
        , update = update
        , model = init testEvents
        }



-- MODEL


type alias Model =
    { events : List Event
    , error : Maybe String
    }


init : Result String (List Event) -> Model
init result =
    case result of
        Ok events ->
            Model events Nothing

        Err err ->
            Model [] (Just err)



-- UPDATE


update : a -> Model -> Model
update msg model =
    model



-- VIEW


view : Model -> Html msg
view { error, events } =
    div []
        [ div [ class "logo" ] []
        , h1 [] [ text "Schedule" ]
        , div [] [ text <| Maybe.withDefault "" error ]
        , hourLabels
        , div [ css [ Css.position Css.relative ] ] <|
            (List.map swimlaneLabel swimlaneNames)
                ++ (eventsView events)
        ]


swimlaneLabel : String -> Html msg
swimlaneLabel title =
    div [ class "swimlane" ]
        [ div [ class "label" ] [ text <| title ]
        ]


eventsView : List Event -> List (Html msg)
eventsView events =
    eventsBySwimlane events
        |> List.map layoutSwimlane
        |> adjustRows 3 0
        |> List.concat
        |> List.map eventView


eventView : PositionedEvent -> Html msg
eventView object =
    let
        event =
            object.event

        h1 =
            1 + Date.hour event.start - 10

        h2 =
            1 + Date.hour event.end - 10

        eventHeight =
            config.rowHeight - config.rowPadding

        hourWidth =
            config.hourWidth
    in
        div
            [ class "event"
            , css
                [ Css.position Css.absolute
                , Css.backgroundColor (Css.rgb 250 50 250)
                , Css.top (Css.px <| toFloat <| object.row * config.rowHeight)
                , Css.height (Css.px <| toFloat <| eventHeight)
                , Css.left (Css.px <| toFloat <| h1 * hourWidth)
                , Css.width (Css.px <| toFloat <| (h2 - h1) * hourWidth - 10)
                ]
            ]
            [ text event.title ]


config =
    { rowHeight = 40
    , rowPadding = 10
    , hourWidth = 100
    }


hourLabels : Html msg
hourLabels =
    let
        hourLabel h =
            div [] [ text <| flip (++) ":00" <| toString <| ((h - 1) % 12) + 1 ]
    in
        div [ class "hours" ] <|
            div [ class "location" ] []
                :: (List.map hourLabel <| List.range 10 21)



-- LAYOUT


type alias PositionedEvent =
    { event : Event, row : Int }


adjustRows : Int -> Int -> List (List PositionedEvent) -> List (List PositionedEvent)
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


layoutSwimlane : List Event -> List PositionedEvent
layoutSwimlane events =
    let
        layoutRow =
            List.map (\e -> PositionedEvent e 0)
    in
        lanes events
            |> List.map layoutRow
            |> adjustRows 1 0
            |> List.concat


lanes : List Event -> List (List Event)
lanes events =
    let
        addToRow : Event -> List (List Event) -> List (List Event)
        addToRow event rows =
            case rows of
                [] ->
                    [ [ event ] ]

                r :: rs ->
                    if List.any (\e -> eventsOverlap event e) r then
                        r :: addToRow event rs
                    else
                        (event :: r) :: rs

        eventsOverlap : Event -> Event -> Bool
        eventsOverlap e1 e2 =
            (Date.toTime e1.start < Date.toTime e2.end)
                && (Date.toTime e2.start < Date.toTime e1.end)
    in
        List.foldl addToRow [] events



-- EVENT TYPE


type alias Event =
    { start : Date
    , end : Date
    , title : String
    , location : Maybe String
    , labels : List String
    }


findSwimlaneEvents : Maybe String -> List Event -> List Event
findSwimlaneEvents laneName events =
    List.filter (.location >> (==) laneName) events


eventsBySwimlane : List Event -> List (List Event)
eventsBySwimlane events =
    List.map (\name -> findSwimlaneEvents (Just name) events) swimlaneNames



-- DECODING


eventDecoder : Json.Decode.Decoder Event
eventDecoder =
    decode Event
        |> required "start" dateDecoder
        |> required "end" dateDecoder
        |> required "title" Json.Decode.string
        |> optional "location" Json.Decode.string
        |> required "labels" (Json.Decode.list Json.Decode.string)


testEvents : Result String (List Event)
testEvents =
    Json.Decode.decodeString (Json.Decode.list eventDecoder) json



-- CONFIGURATION
-- swimlanes : Result String (List String)
-- swimlanes =
--     events
--         |> Result.map (List.map .location)
--         |> Result.map (List.filterMap identity)
--         |> Result.map Set.fromList
--         |> Result.map Set.toList


swimlaneNames : List String
swimlaneNames =
    [ "Entrance", "Upper Level", "Down Stairs", "Work Room" ]
