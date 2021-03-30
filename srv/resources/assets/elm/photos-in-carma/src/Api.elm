module Api exposing (..)

import HttpBuilder
import Http
import Types
import Json.Decode as Decode
import Json.Decode.Pipeline as JsonPipeline

apiGetAttachment : Int -> String
apiGetAttachment n = 
    "/_/Attachment/" ++ String.fromInt n

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