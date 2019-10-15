module SmartSelect.Errors exposing (RequestError(..), fromHttpError, httpErrorToReqErrTuple)

import Http


type RequestError
    = SuccessResponseParseError ( String, String )
    | OtherError ( String, String )


fromHttpError : String -> Http.Error -> RequestError
fromHttpError request error =
    case error of
        Http.BadUrl _ ->
            OtherError <| httpErrorToReqErrTuple request error

        Http.Timeout ->
            OtherError <| httpErrorToReqErrTuple request error

        Http.NetworkError ->
            OtherError <| httpErrorToReqErrTuple request error

        Http.BadStatus _ ->
            OtherError <| httpErrorToReqErrTuple request error

        Http.BadBody _ ->
            SuccessResponseParseError <| httpErrorToReqErrTuple request error


httpErrorToReqErrTuple : String -> Http.Error -> ( String, String )
httpErrorToReqErrTuple request error =
    case error of
        Http.BadUrl url ->
            ( request ++ " failed", "Bad url:" ++ url )

        Http.Timeout ->
            ( request ++ " failed", "Time out" )

        Http.NetworkError ->
            ( request ++ " failed", "Network error. Please check your internet connection and try again later." )

        Http.BadStatus statusCode ->
            ( request ++ " failed", "Status code: " ++ String.fromInt statusCode )

        Http.BadBody _ ->
            ( request ++ " succeeded", "Request has succeed but we have failed to parse the response! Fear not, your changes, if any, have been made." )
