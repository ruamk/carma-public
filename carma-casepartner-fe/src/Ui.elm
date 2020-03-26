module Ui exposing
    ( activeTabStyle
    , addressCell
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
    , pageButtonStyle
    , pageIndicatorStyle
    )

import Element exposing (..)
import Element.Background as Bg
import Element.Border as Bd
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes as Attr


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
        , width fill
        , scrollbarY
        , height fill
        ]
        [ mainMenu username buttons
        , el [ centerX, alignTop, height fill ] content
        ]


mainMenu : String -> List (Element msg) -> Element msg
mainMenu username buttons =
    row
        [ width fill
        , alignTop
        , Bg.color colors.menuBg
        ]
        [ el [ Font.color colors.white, paddingXY 16 16 ] <| text "CaRMa"
        , el [ width fill ] <| none
        , row [ height fill, paddingXY 16 0 ] buttons
        , el [ width fill ] <| none
        , el [ Font.color colors.white, paddingXY 16 16 ] <| text username
        ]


tabStyle =
    [ paddingXY 32 0 ]


activeTabStyle =
    tabStyle
        ++ [ Font.semiBold
           , Font.color colors.blue
           , Bg.color colors.white
           , width fill
           , height fill
           ]


inactiveTabStyle =
    tabStyle
        ++ [ Font.underline
           , Font.color colors.gray
           , width fill
           , height fill
           ]


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
    , centerY
    , width fill
    , Font.semiBold
    , Bd.width 1
    , Bd.color colors.gray
    ]


cellAttrs : List (Attribute msg)
cellAttrs =
    [ paddingXY 8 8
    , centerX
    , width fill
    , Bd.width 1
    , Bd.color colors.gray
    ]


headerCell : String -> Element msg
headerCell t =
    el headerCellAttrs <| text t


cell : String -> Element msg
cell t =
    el cellAttrs <| text t


idCell msg caseId services =
    Input.button
        (cellAttrs
            ++ [ Font.semiBold
               , Font.color colors.lightblue
               ]
        )
        { label =
            text <|
                String.fromInt caseId
                    ++ (if services > 1 then
                            " / " ++ String.fromInt services

                        else
                            ""
                       )
        , onPress = Just <| msg caseId
        }


addressCell t =
    el ([ clip ] ++ cellAttrs) <| text t


pageButtonStyle : List (Attribute msg)
pageButtonStyle =
    [ Bd.color colors.menuBg
    , Bd.rounded 16
    , Bd.width 1
    , Font.color colors.menuBg
    , Font.size 16
    , paddingXY 8 4
    ]


pageIndicatorStyle : List (Attribute msg)
pageIndicatorStyle =
    [ Font.size 18
    , Font.color colors.menuBg
    , Font.semiBold
    ]
