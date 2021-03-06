module Main exposing (..)

import Config exposing (config, eventColor, eventLocations)
import Css
import Data exposing (json)
import Date exposing (Date)
import DecoderExtra exposing (dateDecoder, optional)
import GitHubRibbon exposing (ribbon)
import Helpers exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css, src)
import Http
import Json.Decode
import Json.Decode.Pipeline exposing (decode, required)
import Layout exposing (Block)
import List.Extra as List
import Navigation exposing (Location)
import Set


type alias Flags =
    { apiServer : String }


main : Program Flags Model Msg
main =
    Navigation.programWithFlags SetLocation
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { events : List Event
    , error : Maybe String
    }


init : Flags -> Location -> ( Model, Cmd Msg )
init flags loc =
    let
        model =
            Model [] Nothing

        useTestData =
            (loc.hostname == "localhost" || loc.hostname == "127.0.0.1")
                && (loc.search /= "?server")
    in
        case useTestData of
            False ->
                ( model, getEvents flags.apiServer )

            True ->
                ( model, send <| ReceiveEvents parseTestData )


getEvents : String -> Cmd Msg
getEvents apiServer =
    let
        path =
            String.join ""
                [ "/events?start="
                , config.startDate
                , "&end="
                , config.endDate
                ]

        url =
            urlJoin apiServer path
    in
        Http.get url (Json.Decode.list eventDecoder)
            |> Http.send (ReceiveEvents << Result.mapError toString)



-- UPDATE


type Msg
    = SetLocation Location
    | ReceiveEvents (Result String (List Event))


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        SetLocation _ ->
            ( model, Cmd.none )

        ReceiveEvents result ->
            case result of
                Ok events ->
                    ( Model events Nothing, Cmd.none )

                Err err ->
                    ( Model [] (Just err), Cmd.none )



-- VIEW MODEL


type alias Schedule =
    { lanes : List Lane
    , laneless : List (Block Event)
    }


type alias Lane =
    ( String, Layout.Row Event )


makeSchedule : List Event -> Schedule
makeSchedule events =
    let
        mkBlock event =
            let
                t0 =
                    Date.toTime event.start

                t1 =
                    Date.toTime event.end - Date.toTime event.start
            in
                Layout.makeBlock event t0 t1

        mkFullHeightBlock : Event -> Block Event
        mkFullHeightBlock event =
            let
                block =
                    mkBlock event
            in
                { block | rows = 1 }

        laneNames =
            config.lanes
                |> Maybe.withDefault
                    (eventLocations events
                        |> Set.toList
                        |> List.sort
                    )
    in
        { lanes =
            laneNames
                |> List.map (\name -> eventsAtLocation (Just name) events)
                |> List.map (List.map mkBlock)
                |> List.map Layout.layoutLane
                |> List.zip laneNames
        , laneless =
            List.map mkFullHeightBlock <| eventsAtLocation Nothing events
        }


laneRowCount : Lane -> Int
laneRowCount ( _, blocks ) =
    blocks
        |> List.map (\{ row, rows } -> row + rows)
        |> List.maximum
        |> Maybe.withDefault 0



-- VIEW


view : Model -> Html msg
view { error, events } =
    div []
        [ h1 [] [ logo, text "Schedule" ]
        , gitHubRibbon
        , div [ class "error" ] [ text <| Maybe.withDefault "" error ]
        , schedule events
        ]


gitHubRibbon : Html msg
gitHubRibbon =
    fromUnstyled <|
        ribbon
            config.gitHubRepo
            { position = GitHubRibbon.Right, color = GitHubRibbon.Gray }


logo : Html msg
logo =
    img
        [ class "logo"
        , src <| config.logoPath
        ]
        []


schedule : List Event -> Html msg
schedule events =
    let
        sched =
            makeSchedule events

        { lanes, laneless } =
            sched

        laneTops =
            List.range 0 (List.length lanes)
                |> List.map ((*) config.laneHeight)

        totalHeight =
            config.laneHeight * List.length lanes
    in
        if List.isEmpty events then
            div [ class "loading-delay" ] [ text "No events" ]
        else
            div []
                [ hourLabels events
                , div [ css [ Css.position Css.relative ] ] <|
                    zipWith laneView laneTops lanes
                        ++ List.map (eventView True 0 totalHeight) laneless
                ]


hourLabels : List Event -> Html msg
hourLabels events =
    let
        firstHour =
            List.map (Date.hour << .start) events
                |> List.minimum
                |> Maybe.withDefault 9

        lastHour =
            List.map (Date.hour << .end) events
                |> List.maximum
                |> Maybe.withDefault 17

        hourLabel h =
            div [ class "hour" ] [ text <| flip (++) ":00" <| toString <| ((h - 1) % 12) + 1 ]
    in
        div [ class "hours" ]
            (List.map hourLabel <| List.range firstHour lastHour)


laneView : Int -> Lane -> Html msg
laneView rowTop (( name, row ) as lane) =
    let
        rowHeight =
            -- FIXME: why not 2 *
            (config.laneHeight - 1 * config.lanePadding) // laneRowCount lane
    in
        div [ class "lane" ] <|
            [ h2 [] [ text name ]
            ]
                ++ List.map (eventView False rowTop rowHeight) row


eventView : Bool -> Int -> Int -> Block Event -> Html msg
eventView isFullHeight laneTop rowHeight { model, row, rows } =
    let
        getDateHours : Date -> Float
        getDateHours date =
            toFloat (Date.hour date - 10) + (Date.minute date |> toFloat) / 60

        getDateX : Date -> Float
        getDateX date =
            config.laneLabelWidth + config.hourWidth * getDateHours date

        ( left, right ) =
            ( getDateX model.start, getDateX model.end )

        top =
            toFloat (laneTop + row * rowHeight + config.lanePadding)

        height =
            toFloat (rowHeight * rows - config.rowPadding)
    in
        div
            [ class "event"
            , class
                (if isFullHeight then
                    "full-height"
                 else
                    ""
                )
            , css
                [ Css.position Css.absolute
                , (Css.backgroundColor << Css.hex) <| eventColor model
                , (Css.top << Css.px) top
                , (Css.height << Css.px) height
                , (Css.left << Css.px) left
                , (Css.width << Css.px) <| right - left - config.eventRightMargin
                ]
            ]
            [ div [ class "label" ]
                [ text model.title ]
            ]



-- EVENT TYPE


type alias Event =
    { id : String
    , start : Date
    , end : Date
    , title : String
    , location : Maybe String
    , labels : List String
    }


eventsAtLocation : Maybe String -> List Event -> List Event
eventsAtLocation loc events =
    List.filter (.location >> (==) loc) events



-- DECODING


eventDecoder : Json.Decode.Decoder Event
eventDecoder =
    decode Event
        |> required "id" Json.Decode.string
        |> required "start" dateDecoder
        |> required "end" dateDecoder
        |> required "title" Json.Decode.string
        |> optional "location" Json.Decode.string
        |> required "labels" (Json.Decode.list Json.Decode.string)


parseTestData : Result String (List Event)
parseTestData =
    Json.Decode.decodeString (Json.Decode.list eventDecoder) json
