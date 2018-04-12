module Main exposing (..)

import Config exposing (..)
import Css
import Data exposing (json)
import Date exposing (Date)
import DecoderExtra exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css, src)
import Http
import Json.Decode
import Json.Decode.Pipeline as Pipeline exposing (decode, required)
import Layout exposing (..)
import Navigation exposing (Location)
import Task


main : Program Flags Model Msg
main =
    Navigation.programWithFlags (\_ -> Location)
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Flags =
    { apiServer : String }



-- MODEL


type alias Model =
    { events : List Event
    , error : Maybe String
    }


init : Flags -> Location -> ( Model, Cmd Msg )
init flags loc =
    let
        useTestData =
            (loc.hostname == "localhost" || loc.hostname == "127.0.0.1")
                && (loc.search /= "?server")
    in
        case useTestData of
            False ->
                ( Model [] Nothing, getEvents flags.apiServer )

            True ->
                ( Model [] Nothing, send <| ReceiveEvents testData )


getEvents : String -> Cmd Msg
getEvents apiServer =
    let
        url =
            apiServer ++ config.dataPath

        request =
            Http.get url (Json.Decode.list eventDecoder)

        _ =
            Debug.log "re" url
    in
        Http.send (ReceiveEvents << Result.mapError toString) request


send : a -> Cmd a
send =
    Task.succeed >> Task.perform identity



-- UPDATE


type Msg
    = Location
    | ReceiveEvents (Result String (List Event))


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Location ->
            ( model, Cmd.none )

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
        [ h1 [] [ logo, text "Schedule" ]
        , div [ class "error" ] [ text <| Maybe.withDefault "" error ]
        , hourLabels
        , div [ css [ Css.position Css.relative ] ] <|
            (List.map laneLabel config.laneNames)
                ++ (eventsView events)
        ]


logo : Html msg
logo =
    img
        [ class "logo"
        , src <| config.logoPath
        ]
        []


laneLabel : String -> Html msg
laneLabel title =
    div [ class "swimlane" ]
        [ div [ class "label" ] [ text <| title ]
        ]


eventsView : List Event -> List (Html msg)
eventsView events =
    let
        mkBlock : Event -> Block Event
        mkBlock event =
            makeBlock event (Date.toTime event.start) (Date.toTime event.end - Date.toTime event.start)

        mkFullHeightBlock event =
            let
                block =
                    mkBlock event
            in
                { block | rows = 12 }

        locationless =
            List.filter ((==) "" << (Maybe.withDefault "") << .location) events
    in
        eventsByLane events
            |> List.map (List.map mkBlock)
            |> List.map layoutLane
            |> adjustRows 3 0
            |> List.concat
            |> (++) (List.map mkFullHeightBlock locationless)
            |> List.map eventView


eventView : Block Event -> Html msg
eventView block =
    let
        event =
            block.event

        hours date =
            toFloat (Date.hour date - 10) + (Date.minute date |> toFloat) / 60

        xpos date =
            config.laneLabelWidth + config.hourWidth * hours date

        left =
            xpos event.start

        right =
            xpos event.end

        height =
            config.rowHeight * block.rows - config.rowPadding
    in
        div
            [ class "event"
            , css
                [ Css.position Css.absolute
                , Css.backgroundColor (Css.hex <| eventColor event)
                , Css.top (Css.px <| toFloat <| block.row * config.rowHeight)
                , Css.height (Css.px <| toFloat <| height)
                , Css.left (Css.px <| left)
                , Css.width (Css.px <| right - left - config.xMargin)
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



-- EVENT TYPE


type alias Event =
    { id : String
    , start : Date
    , end : Date
    , title : String
    , location : Maybe String
    , labels : List String
    }


findLaneEvents : Maybe String -> List Event -> List Event
findLaneEvents laneName events =
    List.filter (.location >> (==) laneName) events


eventsByLane : List Event -> List (List Event)
eventsByLane events =
    List.map (\name -> findLaneEvents (Just name) events) config.laneNames



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


testData : Result String (List Event)
testData =
    Json.Decode.decodeString (Json.Decode.list eventDecoder) json
