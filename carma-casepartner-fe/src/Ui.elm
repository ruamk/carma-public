module Ui exposing
    ( addressCell
    , cell
    , cellAttrs
    , centerCell
    , colors
    , dateCell
    , empty
    , idCell
    , mainMenu
    , page
    , pageButtonStyle
    , pageIndicatorStyle
    , timeCell
    , viewSpinner
    )

import Bootstrap.Alert as Alert
import Bootstrap.Badge as Badge
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Navbar as Navbar
import Bootstrap.Spinner as Spinner
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Size as Size
import Bootstrap.Utilities.Spacing as Spacing
import Color exposing (rgb255, toCssString)
import FontAwesome.Attributes as Icon
import FontAwesome.Icon as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import ISO8601 as ISO8601 exposing (Time)
import Time as Time
import Utils exposing (formatDate, formatTime)


email : String
email =
    "dpd@ruamc.ru?subject=Предложения/замечания по личному кабинету партнера."


telephone : String
telephone =
    "8-800-200-72-62"


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


empty =
    Html.text ""


page :
    { navbarMsg : Navbar.State -> a
    , logoutMsg : a
    , usermenuMsg : Dropdown.State -> a
    , navbarState : Navbar.State
    , usermenuState : Dropdown.State
    , username : String
    , buttons : List ( Bool, a, String )
    }
    -> Html a
    -> Html a
page { navbarMsg, logoutMsg, usermenuMsg, navbarState, usermenuState, username, buttons } content =
    Grid.row []
        [ Grid.col [ Col.attrs [ Spacing.p0 ] ]
            [ mainMenu
                { navbarMsg = navbarMsg
                , logoutMsg = logoutMsg
                , usermenuMsg = usermenuMsg
                , navbarState = navbarState
                , usermenuState = usermenuState
                , username = username
                , buttons = buttons
                }
            , div [ Spacing.p1 ] [ content ]
            ]
        ]


mainMenu :
    { navbarMsg : Navbar.State -> a
    , logoutMsg : a
    , usermenuMsg : Dropdown.State -> a
    , navbarState : Navbar.State
    , usermenuState : Dropdown.State
    , username : String
    , buttons : List ( Bool, a, String )
    }
    -> Html a
mainMenu { navbarMsg, logoutMsg, usermenuMsg, navbarState, usermenuState, username, buttons } =
    Navbar.config navbarMsg
        |> Navbar.withAnimation
        |> Navbar.primary
        |> Navbar.brand
            [ style "background-color" "yellow"
            , style "font-weight" "800"
            , Spacing.pb2
            , Spacing.pl2
            , Spacing.pr2
            , Spacing.pt0
            ]
            [ text "Р"
            , span [ style "color" "red" ]
                [ text "А" ]
            , text "МК"
            ]
        |> Navbar.items
            (List.map
                (\( active, itemMsg, label ) ->
                    let
                        d v =
                            div
                                [ class "d-inline-block align-middle"
                                , Spacing.p1
                                ]
                                [ v ]
                    in
                    if active then
                        Navbar.itemLinkActive [ href "" ] [ d <| text label ]

                    else
                        Navbar.itemLink
                            [ href ""
                            , onClick itemMsg
                            ]
                            [ d <| text label ]
                )
                buttons
            )
        |> Navbar.customItems
            [ Navbar.customItem <|
                Button.linkButton
                    [ Button.attrs [ href <| "mailto:" ++ email ]
                    , Button.primary
                    ]
                    [ div
                        [ class "d-inline-block align-middle"
                        , Spacing.p1
                        ]
                        [ Icon.envelope
                            |> Icon.present
                            |> Icon.styled [ Icon.lg ]
                            |> Icon.view
                        ]
                    ]
            , Navbar.customItem <|
                Button.linkButton
                    [ Button.attrs [ href <| "tel:" ++ telephone ]
                    , Button.primary
                    ]
                    [ div
                        [ class "d-inline-block align-middle"
                        , Spacing.p1
                        ]
                        [ Icon.phoneSquareAlt
                            |> Icon.present
                            |> Icon.styled [ Icon.lg ]
                            |> Icon.view
                        ]
                    , div
                        [ class "d-inline-block align-middle"
                        , Spacing.p1
                        ]
                        [ text telephone ]
                    ]
            , Navbar.customItem <|
                Dropdown.dropdown
                    usermenuState
                    { options = []
                    , toggleMsg = usermenuMsg
                    , toggleButton =
                        Dropdown.toggle [ Button.primary ] [ text username ]
                    , items =
                        [ Dropdown.buttonItem [ onClick logoutMsg ] [ text "Выход" ]
                        ]
                    }
            ]
        |> Navbar.view navbarState


cellAttrs : List (Attribute msg)
cellAttrs =
    []


cell : String -> Html msg
cell t =
    div cellAttrs [ text t ]


centerCell : String -> Html msg
centerCell t =
    div (cellAttrs ++ [ class "text-center" ]) [ text t ]


idCell msg serviceId caseId serviceSerial =
    Button.button
        [ Button.attrs [ onClick <| msg serviceId ]
        , Button.roleLink
        , Button.attrs [ Spacing.pb1, Spacing.pt1, Spacing.pl1, Spacing.pr1 ]
        ]
        [ text <|
            String.fromInt caseId
                ++ (if serviceSerial > 1 then
                        "/" ++ String.fromInt serviceSerial

                    else
                        ""
                   )
        ]


timeCell : Maybe Time -> Html msg
timeCell t =
    case t of
        Nothing ->
            text ""

        Just tt ->
            let
                ( date, time ) =
                    formatTime tt
            in
            div cellAttrs
                [ text date
                , br [] []
                , text time
                ]


dateCell : Maybe Time -> Html msg
dateCell t =
    case t of
        Nothing ->
            text ""

        Just tt ->
            div cellAttrs [ text <| formatDate tt ]


addressCell t =
    div cellAttrs [ text t ]


pageButtonStyle : List (Attribute msg)
pageButtonStyle =
    []


pageIndicatorStyle : List (Attribute msg)
pageIndicatorStyle =
    []


viewSpinner : String -> Html msg
viewSpinner size =
    Spinner.spinner
        [ Spinner.attrs
            [ style "width" size
            , style "height" size
            ]
        ]
        []
