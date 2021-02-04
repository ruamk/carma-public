module Components exposing (layout)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Utilities.Size as Size
import Browser exposing (Document)
import FontAwesome.Styles as Icon


layout : { page : Document msg } -> Document msg
layout { page } =
    { title = page.title
    , body =
        [ Grid.containerFluid [ Size.h100 ]
            (CDN.stylesheet
                :: Icon.css
                :: page.body
            )
        ]
    }
