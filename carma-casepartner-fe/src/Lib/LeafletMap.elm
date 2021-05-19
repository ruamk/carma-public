module Lib.LeafletMap exposing (..)

import Html exposing (Html, Attribute)
import Html.Attributes as A


view : List (Attribute msg) -> List (Html msg) -> Html msg
view = 
    Html.node "leaflet-map"


longitude : Float -> Attribute msg
longitude = 
    A.attribute "longitude" << String.fromFloat


latitude : Float -> Attribute msg
latitude = 
    A.attribute "latitude" << String.fromFloat


zoom : Float -> Attribute msg
zoom = 
    A.attribute "zoom" << String.fromFloat

className : String -> Attribute msg
className = 
    A.attribute "class-name"


marker : List (Attribute msg) -> List (Html msg) -> Html msg
marker = 
    Html.node "leaflet-marker"


iconHeight : Int -> Attribute msg
iconHeight = 
    A.attribute "icon-height" << String.fromInt


iconWidth : Int -> Attribute msg
iconWidth = 
    A.attribute "icon-width" << String.fromInt

html : String -> Attribute msg
html = 
    A.attribute "html"