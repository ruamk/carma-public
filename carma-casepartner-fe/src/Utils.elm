module Utils exposing
    ( formatDate
    , formatTime
    , fromUrl
    , WithToast
    , withToast
    , noToast
    )

import Generated.Route as Route exposing (Route)
import ISO8601 exposing (Time)
import Url exposing (Url)
import MessageToast exposing (MessageToast)


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


type alias WithToast msg a = (Maybe (MessageToast msg), a)


withToast : MessageToast msg -> a -> WithToast msg a
withToast toast a = 
    (Just toast, a)


noToast : a -> WithToast msg a
noToast a = 
    (Nothing, a)


