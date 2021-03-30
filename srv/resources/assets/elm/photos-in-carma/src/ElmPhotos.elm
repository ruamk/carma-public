module ElmPhotos exposing (..)

import Api
import Bootstrap.Accordion as Accordion
import Bootstrap.Card.Block as CardBlock
import Bootstrap.Grid as Grid exposing (Column)
import Bootstrap.Grid.Col as Col
import Browser
import Html exposing (..)
import Html.Attributes as A
    exposing
        ( class
        , href
        , style
        )
import Http
import Parser exposing ((|.), (|=))
import Types exposing (Attachment, AttachmentId)


type alias Model =
    { photos : List Photo
    , photosAccordion : Accordion.State
    , attachments : List Attachment
    }


type alias Photo =
    { serviceId : Int
    , image : String
    , latitude : Float
    , longitude : Float
    , created : String
    , photoType : String
    }


type Message
    = PhotosAccordionMsg Accordion.State
    | GotAttachment (Result Http.Error Attachment)


init : String -> ( Model, Cmd.Cmd Message )
init attachments =
    ( { photos =
            [ { serviceId = 0
              , image = "https://sun9-4.userapi.com/impf/c847221/v847221903/1953e7/qqtZDMZjldI.jpg?size=1000x717&quality=96&sign=a028d78653068cd80e8daef52108cb96&type=album"
              , latitude = 0
              , longitude = 0
              , created = ""
              , photoType = "order"
              }
            ]
      , photosAccordion = Accordion.initialState
      , attachments = []
      }
    , Cmd.batch (List.map fetchAttachment <| parseAttachments attachments)
    )


subscriptions : Model -> Sub Message
subscriptions model =
    Accordion.subscriptions model.photosAccordion PhotosAccordionMsg


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        PhotosAccordionMsg newState ->
            ( { model | photosAccordion = newState }
            , Cmd.none
            )

        GotAttachment result ->
            case result of
                Ok attachment ->
                    ( { model
                        | attachments = model.attachments ++ [ attachment ]
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( model
                    , Cmd.none
                    )


view : Model -> Html Message
view model =
    viewPhotosAccordionNEW model


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
        }



-- "Attachment:261961,Attachment:261962"
--   => [261961, 261962]


parseAttachments : String -> List AttachmentId
parseAttachments s =
    let
        attachment =
            Parser.succeed identity
                |. Parser.keyword "Attachment"
                |. Parser.symbol ":"
                |= Parser.int

        attachments =
            Parser.sequence
                { start = ""
                , separator = ","
                , end = ""
                , spaces = Parser.spaces
                , item = attachment
                , trailing = Parser.Forbidden
                }
    in
    Parser.run attachments s
        |> Result.withDefault []


viewPhotos : List Photo -> List (Column Message)
viewPhotos photos =
    let
        colOptions =
            [ Col.sm3
            , Col.attrs
                [ style "display" "flex"
                , style "align-items" "center"
                ]
            ]

        viewPhoto photo =
            Grid.col colOptions
                [ a [ A.href photo.image, A.target "_blank" ]
                    [ img
                        [ A.src photo.image
                        , style "width" "100%"
                        , style "height" "100%"
                        , style "max-height" "150px"
                        , class "rounded"
                        , class "border"
                        , class "img-fluid"
                        , class "img-thumbnail"
                        , style "object-fit" "cover"
                        ]
                        []
                    ]
                ]
    in
    List.map viewPhoto photos


viewPhotosAccordion : Model -> Html Message
viewPhotosAccordion model =
    let
        headerStyles =
            [ style "padding" "0"
            ]

        toggleStyles =
            [ style "height" "100%"
            , style "width" "100%"
            , style "margin-top" "4px"
            , style "margin-bottom" "4px"
            , style "text-align" "left"
            ]

        filterByPhotoType : String -> List Photo -> List Photo
        filterByPhotoType photoType =
            List.filter (\photo -> photo.photoType == photoType)

        viewBefore : List Photo -> Html Message
        viewBefore photos =
            div []
                [ h4 [] [ text "Фото до начала заявки" ]
                , Grid.row []
                    (viewPhotos <| filterByPhotoType "before" photos)
                ]

        viewAfter : List Photo -> Html Message
        viewAfter photos =
            div []
                [ h4 [] [ text "Фото после выполнения заявки" ]
                , Grid.row []
                    (viewPhotos <| filterByPhotoType "after" photos)
                ]

        viewDifficult : List Photo -> Html Message
        viewDifficult photos =
            div []
                [ h4 [] [ text "Фото сложностей" ]
                , Grid.row []
                    (viewPhotos <| filterByPhotoType "difficult" photos)
                ]

        viewOrder : List Photo -> Html Message
        viewOrder photos =
            div []
                [ h4 [] [ text "Заказ-наряд" ]
                , Grid.row []
                    (viewPhotos <| filterByPhotoType "order" photos)
                ]

        viewAttachments_ : List Attachment -> Html Message
        viewAttachments_ attachments =
            div []
                [ h4 [] [ text "Вложения РАМК" ]
                , viewAttachments attachments
                ]

        header =
            "Прикрепленные файлы ("
                ++ String.fromInt (countAttachmentsAndPhotos model.attachments model.photos)
                ++ ")"
    in
    Accordion.config PhotosAccordionMsg
        |> Accordion.withAnimation
        |> Accordion.cards
            [ Accordion.card
                { id = "card1"
                , options = []
                , header =
                    Accordion.toggle toggleStyles [ text header ]
                        |> Accordion.headerH4 headerStyles
                , blocks =
                    [ Accordion.block
                        [ if model.photos == [] then
                            CardBlock.attrs [ class "sm-4" ]

                          else
                            CardBlock.attrs []
                        ]
                        [ CardBlock.custom <|
                            div []
                                [ viewAttachments_ model.attachments
                                , viewBefore model.photos
                                , viewAfter model.photos
                                , viewDifficult model.photos
                                , viewOrder model.photos
                                ]
                        ]
                    ]
                }
            ]
        |> Accordion.view model.photosAccordion


viewPhotosAccordionNEW : Model -> Html Message
viewPhotosAccordionNEW model =
    let
        filterByPhotoType : String -> List Photo -> List Photo
        filterByPhotoType photoType =
            List.filter (\photo -> photo.photoType == photoType)

        viewBefore : List Photo -> Html Message
        viewBefore photos =
            if List.isEmpty <| filterByPhotoType "before" photos then
                text ""

            else
                div [ style "padding-left" "5px" ]
                    [ h4 [] [ text "Фото до начала заявки" ]
                    , Grid.row []
                        (viewPhotos <| filterByPhotoType "before" photos)
                    ]

        viewAfter : List Photo -> Html Message
        viewAfter photos =
            if List.isEmpty <| filterByPhotoType "after" photos then
                text ""

            else
                div [ style "padding-left" "5px" ]
                    [ h4 [] [ text "Фото после выполнения заявки" ]
                    , Grid.row []
                        (viewPhotos <| filterByPhotoType "after" photos)
                    ]

        viewDifficult : List Photo -> Html Message
        viewDifficult photos =
            if List.isEmpty <| filterByPhotoType "difficult" photos then
                text ""

            else
                div [ style "padding-left" "5px" ]
                    [ h4 [] [ text "Фото сложностей" ]
                    , Grid.row []
                        (viewPhotos <| filterByPhotoType "difficult" photos)
                    ]

        viewOrder : List Photo -> Html Message
        viewOrder photos =
            if List.isEmpty <| filterByPhotoType "order" photos then
                text ""

            else
                div [ style "padding-left" "5px" ]
                    [ h4 [] [ text "Заказ-наряд" ]
                    , Grid.row []
                        (viewPhotos <| filterByPhotoType "order" photos)
                    ]

        viewAttachments_ : List Attachment -> Html Message
        viewAttachments_ attachments =
            if List.isEmpty attachments then
                text ""

            else
                div [ style "padding-left" "5px" ]
                    [ h4 [] [ text "Вложения РАМК" ]
                    , viewAttachments attachments
                    ]

        header =
            "Прикрепленные файлы ("
                ++ String.fromInt (countAttachmentsAndPhotos model.attachments model.photos)
                ++ ")"
    in
    div [ class "panel panel-default" ]
        [ div [ class "panel-heading" ]
            [ a [ A.attribute "data-toggle" "collapse", href "#elm-attachments-body" ] [ text header ]
            ]
        , div [ class "panel-collapse collapse", A.id "elm-attachments-body" ]
            [ viewAttachments_ model.attachments
            , viewBefore model.photos
            , viewAfter model.photos
            , viewDifficult model.photos
            , viewOrder model.photos
            ]
        ]


viewAttachments : List Attachment -> Html Message
viewAttachments attachments =
    let
        folderIconSpan =
            Html.span [ class "glyphicon glyphicon-folder-open" ] []

        fileRef attachment =
            let
                ref =
                    "/s/fileupload/attachment/"
                        ++ String.fromInt attachment.id
                        ++ "/"
                        ++ attachment.filename
            in
            a [ href ref ] [ text attachment.filename ]
    in
    attachments
        |> List.map (\attachment -> Html.li [] [ folderIconSpan, Html.text " ", fileRef attachment ])
        |> Html.ul [ class "unstyled" ]



{-
   ul.unstyled(data-bind="foreach: safelyGet(\'filesReference\')")
   li
     span.glyphicon.glyphicon-folder-open
     | &nbsp;
     a.file-name(data-bind="attr: { href: filenameUrl }, text: filename")
-}
-- HELPERS


fetchAttachment : Int -> Cmd Message
fetchAttachment n =
    Api.getAttachment n GotAttachment


countAttachmentsAndPhotos : List Attachment -> List Photo -> Int
countAttachmentsAndPhotos attachments photos =
    List.length attachments + List.length photos
