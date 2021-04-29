module Utils exposing
    ( formatDate
    , formatTime
    , fromUrl
    , sortServices
    , viewColoredMarker
    )

import Generated.Route as Route exposing (Route)
import Html exposing (Attribute, Html, div)
import ISO8601 exposing (Time)
import Svg exposing (circle, svg)
import Svg.Attributes exposing (color, cx, cy, fill, height, r, viewBox, width)
import Types exposing (CurrentCaseInfo)
import Url exposing (Url)


fromUrl : Url -> Route
fromUrl =
    Route.fromUrl >> Maybe.withDefault Route.NotFound


{-| Return: (date\_string, time\_string)
-}
formatTime : Time -> ( String, String )
formatTime nt =
    let
        f : List (Time -> Int) -> List String
        f =
            List.map (\field -> field nt |> String.fromInt |> String.padLeft 2 '0')
    in
    ( String.join "." (f [ .day, .month, .year ])
    , String.join ":" (f [ .hour, .minute ])
    )


{-| Returns: date\_string
-}
formatDate : Time -> String
formatDate nt =
    let
        f : List (Time -> Int) -> List String
        f =
            List.map (\field -> field nt |> String.fromInt |> String.padLeft 2 '0')
    in
    String.join "." (f [ .day, .month, .year ])



-- drop down the services with status `in progress`


sortServices : List CurrentCaseInfo -> List CurrentCaseInfo
sortServices cs =
    let
        ( inProgress, others ) =
            List.partition (\c -> c.cuAccordTime == "В работе") cs

        sortByCallDate : List CurrentCaseInfo -> List CurrentCaseInfo
        sortByCallDate xs =
            let
                emptyTime =
                    { year = 0
                    , month = 0
                    , day = 0
                    , hour = 0
                    , minute = 0
                    , second = 0
                    , millisecond = 0
                    , offset = 0
                    }

                time : CurrentCaseInfo -> ISO8601.Time
                time c =
                    Maybe.withDefault emptyTime c.cuCallDate

                rule : CurrentCaseInfo -> CurrentCaseInfo -> Order
                rule a b =
                    compare
                        (ISO8601.toTime <| time a)
                        (ISO8601.toTime <| time b)
            in
            List.sortWith rule xs
    in
    List.append
        (sortByCallDate others)
        (sortByCallDate inProgress)


viewColoredMarker : Int -> String -> Html msg
viewColoredMarker rad color =
    svg
        [ width <| String.fromInt rad
        , height <| String.fromInt rad
        , viewBox "0 0 120 120"
        ]
        [ circle
            [ fill color
            , r "50%"
            , cx "50%"
            , cy "50%"
            ]
            []
        ]
