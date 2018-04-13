module Main exposing (..)

import Config exposing (config, eventColor)
import Css
import Data exposing (json)
import Date exposing (Date)
import DecoderExtra exposing (dateDecoder, optional)
import GitHubRibbon exposing (ribbon)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css, src)
import Http
import Json.Decode
import Json.Decode.Pipeline exposing (decode, required)
import Layout exposing (Block)
import List.Extra as List
import Navigation exposing (Location)
import Task


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
                { block | rows = config.rowsPerLane * List.length config.lanes }
    in
        { lanes =
            eventsByLane events
                |> List.map (List.map mkBlock)
                |> List.map Layout.layoutLane
                |> Layout.adjustRows config.rowsPerLane 0
                |> List.zip config.lanes
        , laneless =
            List.map mkFullHeightBlock <| eventsAtLocation Nothing events
        }


laneRowCount : Lane -> Int
laneRowCount ( _, row ) =
    let
        top =
            row |> List.map .row |> List.minimum |> Maybe.withDefault 0
    in
        row
            |> List.map (\{ row, rows } -> row + rows)
            |> List.maximum
            |> Maybe.map (\n -> n - top)
            |> Maybe.withDefault 0



-- VIEW


view : Model -> Html msg
view { error, events } =
    div []
        [ h1 [] [ logo, text "Schedule" ]
        , gitHubRibbon
        , div [ class "error" ] [ text <| Maybe.withDefault "" error ]
        , schedule <| makeSchedule events
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


schedule : Schedule -> Html msg
schedule ({ lanes, laneless } as sched) =
    let
        _ =
            lanes
                |> List.map laneRowCount
                |> List.maximum
                |> Maybe.withDefault 0
    in
        div []
            [ hourLabels
            , div [ css [ Css.position Css.relative ] ] <|
                List.map laneLabel config.lanes
                    ++ List.map (laneView config.laneRows) lanes
                    ++ List.map (eventView 0 1.0) laneless
            ]


hourLabels : Html msg
hourLabels =
    let
        hourLabel h =
            div [] [ text <| flip (++) ":00" <| toString <| ((h - 1) % 12) + 1 ]
    in
        div [ class "hours" ] <|
            div [ class "location" ] []
                :: (List.map hourLabel <| List.range 10 21)


laneLabel : String -> Html msg
laneLabel title =
    div [ class "swimlane" ]
        [ div [ class "label" ] [ text <| title ]
        ]


laneView : Int -> Lane -> Html msg
laneView maxRows (( _, row ) as lane) =
    let
        topRow =
            row |> List.map .row |> List.minimum |> Maybe.withDefault 0

        stretch =
            (toFloat maxRows) / (toFloat <| laneRowCount lane)
    in
        div [] <| List.map (eventView topRow stretch) row


eventView : Int -> Float -> Block Event -> Html msg
eventView topRow yScale { model, row, rows } =
    let
        -- _ =
        -- Debug.log "stretch" stretch
        getDateHours : Date -> Float
        getDateHours date =
            toFloat (Date.hour date - 10) + (Date.minute date |> toFloat) / 60

        getDateX : Date -> Float
        getDateX date =
            config.laneLabelWidth + config.hourWidth * getDateHours date

        ( left, right ) =
            ( getDateX model.start, getDateX model.end )

        height =
            config.rowHeight * rows - config.rowPadding

        isFullHeight =
            rows > config.rowsPerLane
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
                , Css.backgroundColor (Css.hex <| eventColor model)
                , Css.top (Css.px <| (+) (toFloat <| topRow * config.rowHeight) <| (*) yScale <| toFloat <| (row - topRow) * config.rowHeight)
                , Css.height (Css.px <| (*) yScale <| toFloat height)
                , Css.left (Css.px left)
                , Css.width (Css.px <| right - left - config.eventRightMargin)
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


eventsByLane : List Event -> List (List Event)
eventsByLane events =
    List.map (\name -> eventsAtLocation (Just name) events) config.lanes



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



-- HELPERS


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
