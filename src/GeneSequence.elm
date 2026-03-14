port module GeneSequence exposing (requestGeneSequence, receiveGeneSequence)

import Json.Encode as E
import Json.Decode as D


port requestGeneSequence : E.Value -> Cmd msg


port receiveGeneSequence : (D.Value -> msg) -> Sub msg
