module Main exposing (..)

import Config exposing (config)
import Css
import Data exposing (json)
import Date exposing (Date)
import DecoderExtra exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css)
import Http
import Json.Decode
import Json.Decode.Pipeline as Pipeline exposing (decode, required)
import Task


main : Program Never Model Msg
main =
    Html.program
        { init = init <| Just testEvents
        , view = view >> toUnstyled
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { events : List Event
    , error : Maybe String
    }


init : Maybe (Result String (List Event)) -> ( Model, Cmd Msg )
init testData =
    case testData of
        Nothing ->
            ( Model [] Nothing, getEvents )

        Just result ->
            ( Model [] Nothing, send <| ReceiveEvents result )


getEvents : Cmd Msg
getEvents =
    let
        request =
            Http.get config.dataUrl (Json.Decode.list eventDecoder)
    in
        Http.send (ReceiveEvents << Result.mapError toString) request


send : a -> Cmd a
send =
    Task.succeed >> Task.perform identity



-- UPDATE


type Msg
    = ReceiveEvents (Result String (List Event))


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        ReceiveEvents result ->
            case result of
                Ok events ->
                    ( Model events Nothing, Cmd.none )

                Err err ->
                    ( Model [] (Just err), Cmd.none )



-- VIEW


view : Model -> Html msg
view { error, events } =
    div []
        [ div [ class "logo" ] []
        , h1 [] [ text "Schedule" ]
        , div [ class "error" ] [ text <| Maybe.withDefault "" error ]
        , hourLabels
        , div [ css [ Css.position Css.relative ] ] <|
            (List.map swimlaneLabel config.swimlaneNames)
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
    List.map (\name -> findSwimlaneEvents (Just name) events) config.swimlaneNames



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
