port module JS exposing (drawGraph, sessionRestore, sessionSave)


port drawGraph : String -> Cmd msg


port sessionSave : String -> Cmd msg


port sessionRestore : (String -> msg) -> Sub msg
