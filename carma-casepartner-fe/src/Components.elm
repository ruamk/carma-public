module Components exposing (layout)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Utilities.Size as Size
import Browser exposing (Document)
import FontAwesome.Styles as Icon
import Generated.Route as Route exposing (Route)
import Html exposing (..)
import Html.Attributes as Attr exposing (class, href, style)


layout : { page : Document msg } -> Document msg
layout { page } =
    { title = page.title
    , body =
        [ Grid.containerFluid [ Size.h100 ]
            ([ CDN.stylesheet -- bootstrap css, required
             , Icon.css -- Font.awesome css, required
             ]
                ++ page.body
            )
        ]
    }


navbar : Html msg
navbar =
    header [ class "row center-y spacing--between" ]
        []


footer : Html msg
footer =
    Html.footer [] [ text "built with elm ❤" ]
