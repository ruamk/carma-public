module Ui exposing
    ( addressCell
    , cell
    , cellAttrs
    , colors
    , idCell
    , mainMenu
    , page
    , pageButtonStyle
    , pageIndicatorStyle
    )

import Bootstrap.Badge as Badge
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Navbar as Navbar
import Bootstrap.Text as Text
import Bootstrap.Utilities.Size as Size
import Bootstrap.Utilities.Spacing as Spacing
import Color exposing (rgb255, toCssString)
import Html exposing (..)
import Html.Attributes as Attrs
import Html.Events exposing (onClick)


colors =
    { green = toCssString <| rgb255 57 132 57
    , white = toCssString <| rgb255 255 255 255
    , black = toCssString <| rgb255 0 0 0
    , red = toCssString <| rgb255 255 0 0
    , gray = toCssString <| rgb255 192 192 192
    , darkgray = toCssString <| rgb255 128 128 128
    , lightgray = toCssString <| rgb255 224 224 224
    , blue = toCssString <| rgb255 0 0 255
    , lightblue = toCssString <| rgb255 64 64 255
    , menuBg = toCssString <| rgb255 0x19 0x76 0xD2
    , menuFg = toCssString <| rgb255 255 255 255
    , casesBg = toCssString <| rgb255 238 246 251
    }


page navbarMsg navbarState username buttons content =
    Grid.row []
        [ Grid.col [ Col.attrs [ Spacing.p0 ] ]
            [ mainMenu navbarMsg navbarState username buttons
            , div [ Spacing.p4 ] [ content ]
            ]
        ]


mainMenu msg state username buttons =
    Navbar.config msg
        |> Navbar.withAnimation
        |> Navbar.info
        |> Navbar.brand []
            [ img
                [ Attrs.src "/logo.png"
                , Attrs.class "d-inline-block align-middle"
                , Attrs.style "width" "60px"
                ]
                []
            , text "CaRMa"
            ]
        |> Navbar.items
            (List.map
                (\( active, itemMsg, label ) ->
                    if active then
                        Navbar.itemLinkActive [ Attrs.href "" ] [ text label ]

                    else
                        Navbar.itemLink
                            [ Attrs.href ""
                            , onClick itemMsg
                            ]
                            [ text label ]
                )
                buttons
            )
        |> Navbar.customItems
            [ Navbar.textItem [] [ text username ] ]
        |> Navbar.view state


cellAttrs : List (Attribute msg)
cellAttrs =
    []


cell : String -> Html msg
cell t =
    div cellAttrs [ text t ]


idCell msg caseId services =
    Button.button
        [ Button.attrs [ onClick <| msg caseId ]
        , Button.roleLink
        , Button.attrs [ Spacing.pb0, Spacing.pt0 ]
        ]
        [ text <|
            String.fromInt caseId
                ++ (if services > 1 then
                        " / " ++ String.fromInt services

                    else
                        ""
                   )
        ]


addressCell t =
    div cellAttrs [ text t ]


pageButtonStyle : List (Attribute msg)
pageButtonStyle =
    []


pageIndicatorStyle : List (Attribute msg)
pageIndicatorStyle =
    []
