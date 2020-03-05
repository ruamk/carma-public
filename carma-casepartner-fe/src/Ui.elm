module Ui exposing
    ( activeTabStyle
    , button
    , casesTableStyle
    , cell
    , cellAttrs
    , colors
    , headerCell
    , headerCellAttrs
    , horizontalLine
    , idCell
    , inactiveTabStyle
    , mainMenu
    , page
    )

import Element exposing (..)
import Element.Background as Bg
import Element.Border as Bd
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as Attr
import Html



{-
   colors :
       { green : Element.Color
       , white : Element.Color
       , black : Element.Color
       , red : Element.Color
       , gray : Element.Color
       , darkgray : Element.Color
       , lightgray : Element.Color
       , lightblue : Element.Color
       }
-}


colors =
    { green = rgb255 57 132 57
    , white = rgb255 255 255 255
    , black = rgb255 0 0 0
    , red = rgb255 255 0 0
    , gray = rgb255 192 192 192
    , darkgray = rgb255 128 128 128
    , lightgray = rgb255 224 224 224
    , blue = rgb255 0 0 255
    , lightblue = rgb255 64 64 255
    , menuBg = rgb255 0x19 0x76 0xD2
    , menuFg = rgb255 255 255 255
    }


horizontalLine : Element msg
horizontalLine =
    el [ Bg.color colors.gray, width fill, height <| px 1 ] none


button : ( String, Maybe msg ) -> Element msg
button ( label, onPress ) =
    Input.button
        [ Bg.color colors.green
        , Bd.color colors.green
        , Font.color colors.white
        , paddingXY 24 8
        , Bd.rounded 4
        , Bd.width 2
        , Font.size 16
        , mouseOver
            [ Bg.color colors.white
            , Font.color colors.green
            ]
        , transition 200 [ "color", "background-color" ]
        ]
        { onPress = onPress
        , label = text label
        }


transition : Int -> List String -> Attribute msg
transition duration properties =
    Element.htmlAttribute <|
        Attr.style
            "transition"
            (properties
                |> List.map (\prop -> prop ++ " " ++ String.fromInt duration ++ "ms ease-in-out")
                |> String.join ", "
            )




page : String -> List (Element msg) -> Element msg -> Element msg
page username buttons content =
    column
        [ centerX
        --, alignTop
        , spacing 0
        , padding 0
        , width fill
        , scrollbarY
        , height fill
        ]
        [ mainMenu username buttons
        , el [centerX, alignTop, height fill ] content
        ]


mainMenu : String -> List (Element msg) -> Element msg
mainMenu username buttons =
    row
        [ spacingXY 32 8
        , paddingXY 16 16
        , width fill
        , alignTop
        , Bg.color colors.menuBg
        ]
        ([ el [ Font.color colors.white ] <| text "CaRMa"
         , el [ width fill ] <| none
         ]
            ++ buttons
            ++ [ el [ width fill ] <| none
               , el [ Font.color colors.white ] <| text username
               ]
        )


casesTableStyle : List (Attribute msg)
casesTableStyle =
    [ Font.size 12
    , Bg.color colors.lightgray
    , Bd.width 1
    , Bd.color colors.gray
    ]


headerCellAttrs : List (Attribute msg)
headerCellAttrs =
    [ paddingXY 8 8
    , width fill
    , Font.semiBold
    , Bd.width 1
    , Bd.color colors.gray
    ]


cellAttrs : List (Attribute msg)
cellAttrs =
    [ paddingXY 8 8
    , centerX
    , Bd.width 1
    , Bd.color colors.gray
    ]


headerCell : String -> Element msg
headerCell t =
    el headerCellAttrs <| text t


cell : String -> Element msg
cell t =
    el cellAttrs <| text t


idCell msg t =
    Input.button
        (cellAttrs
            ++ [ Font.underline
               , Font.semiBold
               , Font.color colors.lightblue
               ]
        )
        { label = text t
        , onPress = Just <| msg t
        }



--tabs : List (String, msg) -> Element msg
--tabs buttons =
--    List.maps


activeTabStyle =
    [ Font.semiBold, Font.color colors.white ]


inactiveTabStyle =
    [ Font.underline, Font.color colors.gray ]
