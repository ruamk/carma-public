module Utils exposing
    ( formatDate
    , formatTime
    , fromUrl
    , sortServices
    )

import Generated.Route as Route exposing (Route)
import ISO8601 exposing (Time)
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


-- drop down the services with status `in progress` and `safe storing`
sortServices : List CurrentCaseInfo -> List CurrentCaseInfo
sortServices cs =
    let
        (bottom, up) =  
            let 
                bottomTypes = 
                    ["В работе", "На хранении"]
            in
            List.partition (\c -> List.member c.cuAccordTime bottomTypes) cs

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
        (sortByCallDate up)
        (sortByCallDate bottom)

