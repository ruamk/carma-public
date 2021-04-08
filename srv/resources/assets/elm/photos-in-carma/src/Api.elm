module Api exposing (..)

import Http
import HttpBuilder
import Json.Decode as Decode
import Json.Decode.Pipeline as JsonPipeline
import Types


apiGetAttachment : Int -> String
apiGetAttachment n =
    "/_/Attachment/"
        ++ String.fromInt n


apiGetPhotos : Int -> String
apiGetPhotos serviceId =
    "/proxy/casepartner/case/"
        ++ String.fromInt serviceId
        ++ "/photo"


getAttachment : Int -> (Result Http.Error Types.Attachment -> msg) -> Cmd msg
getAttachment attachmentId message =
    let
        attachmentDecoder =
            Decode.succeed Types.Attachment
                |> JsonPipeline.required "hash" Decode.string
                |> JsonPipeline.required "ctime" Decode.string
                |> JsonPipeline.required "id" Decode.int
                |> JsonPipeline.required "filename" Decode.string
    in
    HttpBuilder.get (apiGetAttachment attachmentId)
        |> HttpBuilder.withExpect (Http.expectJson message attachmentDecoder)
        |> HttpBuilder.request


getPhotos : Int -> (Result Http.Error (Result String (List Types.Photo)) -> msg) -> Cmd msg
getPhotos serviceId message =
    let
        photoDecoder =
            Decode.succeed Types.Photo
                |> JsonPipeline.required "serviceId" Decode.int
                |> JsonPipeline.required "image" Decode.string
                |> JsonPipeline.required "latitude" Decode.float
                |> JsonPipeline.required "longitude" Decode.float
                |> JsonPipeline.required "created" Decode.string
                |> JsonPipeline.required "type" Decode.string

        decoder =
            let
                handleStatus status =
                    case status of
                        "ok" ->
                            Decode.map Ok <| Decode.field "message" (Decode.list photoDecoder)

                        err ->
                            Decode.map Err <| Decode.succeed err
            in
            Decode.field "status" Decode.string
                |> Decode.andThen handleStatus
    in
    HttpBuilder.get (apiGetPhotos serviceId)
        |> HttpBuilder.withExpect (Http.expectJson message decoder)
        |> HttpBuilder.request
