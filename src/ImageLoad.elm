module ImageLoad exposing (load, Error(..))

import Native.ImageLoad
import Task exposing (Task)
import Json.Decode exposing (Decoder)


type Error
    = LoadError
    | DecodeError


load : String -> Decoder a -> Task Error a
load =
    Native.ImageLoad.load
