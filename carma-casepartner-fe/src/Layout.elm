module Layout exposing (view)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Size as Size
import FontAwesome.Styles as Icon
import Generated.Routes as Routes exposing (Route, routes)
import Html exposing (..)
import Html.Attributes as Attrs
import Utils.Spa as Spa


view : Spa.LayoutContext msg -> Html msg
view { page, route } =
    Grid.containerFluid [ Size.h100 ]
        [ CDN.stylesheet -- bootstrap css, required
        , Icon.css -- FontAwesome css, required
        , page
        ]
