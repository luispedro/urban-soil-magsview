port module GeneSequence exposing (requestGeneSequence, receiveGeneSequence, copyToClipboard, downloadGeneFasta, requestRRnaGenes, receiveRRnaGenes)

import Json.Encode as E
import Json.Decode as D


port requestGeneSequence : E.Value -> Cmd msg


port receiveGeneSequence : (D.Value -> msg) -> Sub msg


port copyToClipboard : E.Value -> Cmd msg


port downloadGeneFasta : E.Value -> Cmd msg


port requestRRnaGenes : E.Value -> Cmd msg


port receiveRRnaGenes : (D.Value -> msg) -> Sub msg
