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
import Json.Decode as Decode
import Json.Decode.Pipeline as JsonPipeline
import Parser exposing ((|.), (|=))
import Types exposing (Attachment, AttachmentId, Photo)


type alias Model =
    { photos : List Photo
    , photosAccordion : Accordion.State
    , attachments : List Attachment
    , serviceId : Int
    }


type alias Flags =
    { attachments : String
    , serviceid : Int
    }


type Message
    = GotAttachment (Result Http.Error Attachment)
    | GotPhotos (Result Http.Error (Result String (List Photo)))


init : Decode.Value -> ( Model, Cmd.Cmd Message )
init flags =
    let
        attachmentsIds =
            flags
                |> Decode.decodeValue (Decode.field "attachments" Decode.string)
                |> Result.withDefault ""
                |> parseAttachments

        serviceId =
            flags
                |> Decode.decodeValue (Decode.field "serviceId" Decode.string)
                |> Result.withDefault ""
                |> String.toInt
                |> Maybe.withDefault 0

        getAttachments =
            attachmentsIds
                |> List.map (\a -> Api.getAttachment a GotAttachment)
                |> Cmd.batch
    in
    ( { photos = []
      , photosAccordion = Accordion.initialState
      , attachments = []
      , serviceId = serviceId
      }
    , Cmd.batch
        [ getAttachments
        , Api.getPhotos serviceId GotPhotos
        ]
    )


subscriptions : Model -> Sub Message
subscriptions model =
    Sub.none


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
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

        GotPhotos result ->
            case result of
                Ok result2 ->
                    case result2 of
                        Ok photos ->
                            ( { model
                                | photos = photos
                              }
                            , Cmd.none
                            )

                        Err error ->
                            ( model
                            , Cmd.none
                            )

                Err error ->
                    ( model
                    , Cmd.none
                    )


view : Model -> Html Message
view model =
    viewPhotosAccordion model


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
                        , class "rounded border img-fluid img-thumbnail"
                        , style "width" "100%"
                        , style "height" "100%"
                        , style "max-height" "150px"
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



-- HELPERS


fetchAttachment : Int -> Cmd Message
fetchAttachment n =
    Api.getAttachment n GotAttachment


countAttachmentsAndPhotos : List Attachment -> List Photo -> Int
countAttachmentsAndPhotos attachments photos =
    List.length attachments + List.length photos
