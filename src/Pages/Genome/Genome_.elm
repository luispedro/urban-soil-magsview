module Pages.Genome.Genome_ exposing (Model, Msg, page)

import Html
import Html.Attributes as HtmlAttr
import Html.Events as HE

import Effect exposing (Effect)
import Route exposing (Route)
import Page exposing (Page)
import View exposing (View)

import Http
import Json.Decode as D
import Json.Encode as E

import Svg
import Svg.Attributes as SvgAttr
import Svg.Events as SvgEvents

import W.InputCheckbox as InputCheckbox
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Table as Table


import Shared
import Data exposing (mags)
import DataModel exposing (MAG)
import Layouts
import GenomeStats exposing (taxonomyLast, printableTaxonomy, showTaxon)
import Utils exposing (mkTooltipQuestionMark)
import Downloads exposing (mkFASTALink, mkENOGLink, mkEMapperSummaryLink)
import GeneSequence

-- INIT

type alias MicrobeAtlasMatch =
    { seq : String
    , otu : String
    }

type alias ARG =
    { seq : String
    , argName : String
    , aro : String
    , cutOff : String
    , matchID : Float
    , fractionMatch : Float
    , inResfinder : Bool
    , drugClass : String
    }

type alias MAGData =
    { microbeAtlas : List MicrobeAtlasMatch
    , argData : List ARG
    }

type alias EMapperGene =
    { seqid : String
    , contig : String
    , start : Int
    , end : Int
    , strand : String
    , cogCategory : String
    , preferredName : String
    , keggKo : List String
    , keggModule : List String
    }

type LoadedDataModel =
    Loaded MAGData
    | LoadError String
    | Waiting

type EMapperDataModel
    = EMapperLoaded (List EMapperGene)
    | EMapperError String
    | EMapperWaiting

type alias GeneSequenceData =
    { dna : String
    , protein : String
    }

type GeneSequenceState
    = NoGeneSelected
    | GeneSequenceLoading EMapperGene
    | GeneSequenceLoaded EMapperGene GeneSequenceData
    | GeneSequenceError EMapperGene String

type alias Model =
    { magdata : LoadedDataModel
    , emapperData : EMapperDataModel
    , expanded : Bool
    , expanded16S : List Int
    , showARGSequences : Bool
    , genomeMapZoom : Float
    , genomeMapOffset : Float
    , magId : String
    , geneSequence : GeneSequenceState
    , showGeneTable : Bool
    , showDnaSequence : Bool
    }

type Msg =
    ResultsData (Result Http.Error APIResult)
    | EMapperData (Result Http.Error String)
    | Toggle16SExpanded
    | Expand16S Int
    | ToggleShowARGSequences
    | GenomeMapZoomIn
    | GenomeMapZoomOut
    | GenomeMapZoomReset
    | GenomeMapScrollLeft
    | GenomeMapScrollRight
    | GeneClicked EMapperGene
    | GeneSequenceReceived D.Value
    | CloseGeneDetail
    | CopyToClipboard String String
    | ToggleGeneTable
    | ToggleDnaSequence
    | NoMsg

type APIResult =
    APIError String | APIResultOK MAGData

type alias MightMAG
    = Result String MAG

decodeARG : D.Decoder ARG
decodeARG =
    D.map8 ARG
        (D.field "Sequence" D.string)
        (D.field "ArgName" D.string)
        (D.field "ARO" D.string)
        (D.field "Cut_Off" D.string)
        (D.field "Identity" D.float)
        (D.field "Coverage" D.float)
        (D.field "InResfinder" D.bool)
        (D.field "Drug Class" D.string)

decodeMAGData : D.Decoder APIResult
decodeMAGData =
    D.map2 MAGData
        (D.field "16S" (D.list (D.map2 MicrobeAtlasMatch
            (D.field "Seq" D.string)
            (D.field "OTU" D.string)
        )))
        (D.field "ARGs" (D.list decodeARG))
    |> D.map APIResultOK

model0 : String -> Model
model0 magid =
    { magdata = Waiting
    , emapperData = EMapperWaiting
    , expanded = False
    , expanded16S = []
    , showARGSequences = False
    , genomeMapZoom = 1.0
    , genomeMapOffset = 0.0
    , magId = magid
    , geneSequence = NoGeneSelected
    , showGeneTable = False
    , showDnaSequence = False
    }

cmd0 : String -> Effect Msg
cmd0 magid =
    Effect.batch
        [ Effect.sendCmd
            (Http.get
                { url = "/genome-data/"++ magid ++ ".json"
                , expect = Http.expectJson ResultsData decodeMAGData
                }
            )
        , Effect.sendCmd
            (Http.get
                { url = mkEMapperSummaryLink magid
                , expect = Http.expectString EMapperData
                }
            )
        ]

page : Shared.Model -> Route { genome : String } -> Page Model Msg
page shared route =
    Page.new
        { init = \_ -> (model0 route.params.genome, cmd0 route.params.genome)
        , update = update
        , subscriptions = \_ -> GeneSequence.receiveGeneSequence GeneSequenceReceived
        , view = view shared route
        }
    |> Page.withLayout (\_ -> Layouts.Main {})


getMAG : String -> MightMAG
getMAG g =
    mags
        |> List.filter (\m -> m.id == g)
        |> List.head
        |> (\mm -> case mm of
            Just m -> Ok m
            Nothing -> Err ("Genome not found: " ++ g)
        )

update :
    Msg
    -> Model
    -> (Model, Effect Msg)
update msg model =
    case msg of
        ResultsData r ->
            let magdata = case r of
                    Ok (APIResultOK v) -> Loaded v
                    Ok (APIError e) -> LoadError e
                    Err err -> case err of
                        Http.BadUrl s -> LoadError ("Bad URL: "++ s)
                        Http.Timeout  -> LoadError "Timeout"
                        Http.NetworkError -> LoadError ("Network error!")
                        Http.BadStatus s -> LoadError (("Bad status: " ++ String.fromInt s))
                        Http.BadBody s -> LoadError (("Bad body: " ++ s))
            in
                ( { model | magdata = magdata }
                , Effect.none
                )
        EMapperData r ->
            let emapperData = case r of
                    Ok tsv -> case parseEMapperTsv tsv of
                        Ok genes -> EMapperLoaded genes
                        Err e -> EMapperError e
                    Err err -> case err of
                        Http.BadUrl s -> EMapperError ("Bad URL: " ++ s)
                        Http.Timeout -> EMapperError "Timeout"
                        Http.NetworkError -> EMapperError "Network error"
                        Http.BadStatus s -> EMapperError ("Bad status: " ++ String.fromInt s)
                        Http.BadBody s -> EMapperError ("Bad body: " ++ s)
            in
                ( { model | emapperData = emapperData }, Effect.none )
        Toggle16SExpanded ->
            ( { model | expanded = not model.expanded }, Effect.none )
        Expand16S ix ->
            if List.member ix model.expanded16S then
                ({ model | expanded16S = List.filter (\i -> i /= ix) model.expanded16S }, Effect.none)
            else
                ({ model | expanded16S = ix :: model.expanded16S }, Effect.none)
        ToggleShowARGSequences ->
            ( { model | showARGSequences = not model.showARGSequences }
            , Effect.none
            )
        GenomeMapZoomIn ->
            let
                newZoom = Basics.min 50 (model.genomeMapZoom * 1.5)
                newOffset = clampOffset newZoom model.genomeMapOffset
            in
            ( { model | genomeMapZoom = newZoom, genomeMapOffset = newOffset }, Effect.none )
        GenomeMapZoomOut ->
            let
                newZoom = Basics.max 1 (model.genomeMapZoom / 1.5)
                newOffset = clampOffset newZoom model.genomeMapOffset
            in
            ( { model | genomeMapZoom = newZoom, genomeMapOffset = newOffset }, Effect.none )
        GenomeMapZoomReset ->
            ( { model | genomeMapZoom = 1.0, genomeMapOffset = 0.0 }, Effect.none )
        GenomeMapScrollLeft ->
            let
                step = 0.2 / model.genomeMapZoom
                newOffset = clampOffset model.genomeMapZoom (model.genomeMapOffset - step)
            in
            ( { model | genomeMapOffset = newOffset }, Effect.none )
        GenomeMapScrollRight ->
            let
                step = 0.2 / model.genomeMapZoom
                newOffset = clampOffset model.genomeMapZoom (model.genomeMapOffset + step)
            in
            ( { model | genomeMapOffset = newOffset }, Effect.none )

        ToggleGeneTable ->
            ( { model | showGeneTable = not model.showGeneTable }, Effect.none )

        ToggleDnaSequence ->
            ( { model | showDnaSequence = not model.showDnaSequence }, Effect.none )

        GeneClicked gene ->
            ( { model | geneSequence = GeneSequenceLoading gene }
            , Effect.sendCmd
                (GeneSequence.requestGeneSequence
                    (E.object
                        [ ( "magId", E.string model.magId )
                        , ( "contig", E.string gene.contig )
                        , ( "start", E.int gene.start )
                        , ( "end", E.int gene.end )
                        , ( "strand", E.string gene.strand )
                        , ( "seqid", E.string gene.seqid )
                        ]
                    )
                )
            )

        GeneSequenceReceived value ->
            let
                resultDecoder =
                    D.map2 GeneSequenceData
                        (D.field "dna" D.string)
                        (D.field "protein" D.string)
                newState =
                    case D.decodeValue resultDecoder value of
                        Ok seqData ->
                            case model.geneSequence of
                                GeneSequenceLoading gene ->
                                    GeneSequenceLoaded gene seqData
                                _ ->
                                    model.geneSequence
                        Err err ->
                            case D.decodeValue (D.field "error" D.string) value of
                                Ok errorMsg ->
                                    case model.geneSequence of
                                        GeneSequenceLoading gene ->
                                            GeneSequenceError gene errorMsg
                                        _ ->
                                            model.geneSequence
                                Err _ ->
                                    case model.geneSequence of
                                        GeneSequenceLoading gene ->
                                            GeneSequenceError gene (D.errorToString err)
                                        _ ->
                                            model.geneSequence
            in
            ( { model | geneSequence = newState }, Effect.none )

        CloseGeneDetail ->
            ( { model | geneSequence = NoGeneSelected }, Effect.none )

        CopyToClipboard text buttonId ->
            ( model, Effect.sendCmd (GeneSequence.copyToClipboard
                (E.object
                    [ ("text", E.string text)
                    , ("buttonId", E.string buttonId)
                    ])) )

        _ ->
            ( model
            , Effect.none
            )


clampOffset : Float -> Float -> Float
clampOffset zoom offset =
    let
        maxOffset = Basics.max 0 (1 - 1 / zoom)
    in
    Basics.max 0 (Basics.min maxOffset offset)


view :
    Shared.Model
    -> Route { genome : String }
    -> Model
    -> View Msg
view sm route model =
        { title = route.params.genome ++ " - Genome details"
        , body =
            [ Html.div []
                (case getMAG route.params.genome of
                    Err err ->
                        [ Html.text ("Error: "++err) ]
                    Ok mag ->
                        showMag model mag
                )
            , Html.a [ HtmlAttr.href "/genomes" ]
                [ Html.text "Back to genomes" ]
            ]
        }

showWithCommas : Int -> String
showWithCommas n =
    let
        addCommas s =
            if String.length s <= 3
                then s
            else (addCommas (String.slice 0 (String.length s - 3) s)) ++ "," ++ String.slice (String.length s - 3) (String.length s) s
    in
        addCommas (String.fromInt n)

basicTR title value =
    Table.tr []
        [ Table.td []
            [ Html.text title ]
        , Table.td []
            [ Html.text value ]
        ]

showMag : Model -> MAG -> List (Html.Html Msg)
showMag model mag =
    [ Html.h1 []
        [ Html.text ("Genome: " ++ mag.id) ]
    , Grid.simpleRow [
        Grid.col [ ] [
            Html.h3 [] [ Html.text "General QC information" ]
            , Table.table
                    { options = [ Table.striped, Table.hover, Table.responsive ]
                    , thead =  Table.simpleThead
                        [ Table.th []
                            [ Html.text "Property" ]
                        , Table.th []
                            [ Html.text "Value" ]
                        ]
                , tbody = Table.tbody []
                    ([basicTR "Genome ID" mag.id
                    , basicTR "#Contigs" (String.fromInt mag.nrContigs)
                    , basicTR "Genome Size" (showWithCommas mag.genomeSize ++ " bp")
                    , basicTR "Completeness" (String.fromFloat mag.completeness ++ "%")
                    , basicTR "Contamination" (String.fromFloat mag.contamination ++ "%")
                    ] ++ show16S model mag ++ [
                      basicTR "#23s rRNA" (String.fromInt mag.r23sRrna)
                    , basicTR "#5s rRNA" (String.fromInt mag.r5sRrna)
                    , basicTR "#tRNA" (String.fromInt mag.trna)
                ])
        }]
        , Grid.col [ ] [
            Html.h3 [] [ Html.text "Taxonomic classification (GTDB)" ]
            ,Html.div
                [HtmlAttr.style "border-bottom" "2px solid black"]
                (let
                    r : List String -> List (Html.Html Msg)
                    r tax = case tax of
                        [] -> []
                        (x::xs) ->
                            [Html.div [HtmlAttr.style "padding-left" "1em"
                                    , HtmlAttr.style "border-left" "2px solid black"
                                    ]
                                ((showTaxon x)::(r xs))
                            ]
                in r (String.split ";" mag.taxonomy)
                )
            , Html.span []
                (let
                    n = mags
                            |> List.filter (\m -> m.taxonomy == mag.taxonomy)
                            |> List.length
                    rep = mags
                            |> List.filter (\m -> m.taxonomy == mag.taxonomy && m.isRepresentative)
                            |> List.head -- should be at most one
                            |> Maybe.withDefault mag -- if no representative, use this one (but should never happen)
                in
                    if mag.isRepresentative
                        then
                            [Html.span []
                                [ Html.text "This genome is the representative"
                                , if n == 1
                                    then Html.text " (and only) "
                                    else Html.text " "
                                , Html.text "genome for "
                                , Html.em []
                                    [ Html.text (printableTaxonomy mag.taxonomy)
                                    ]
                                , Html.text " in our dataset."
                                , mkTooltipQuestionMark ("The representative genome is the best genome in our data.")
                                ]
                            ]
                        else
                            [Html.text "This is ", Html.strong [] [Html.text "not"], Html.text " the representative genome for "
                            , Html.em []
                                [ Html.text (printableTaxonomy mag.taxonomy)
                                ]
                            , Html.text " in our dataset."
                            , Html.text " The representative genome is "
                                , Html.a [HtmlAttr.href ("/genome/" ++ rep.id)]
                                    [ Html.text rep.id ]
                                , Html.text " with "
                                , Html.em [] [Html.text (String.fromFloat rep.completeness ++ "% completeness")]
                                , Html.text " and "
                                , Html.em [] [Html.text (String.fromFloat rep.contamination ++ "% contamination")]
                                , Html.text "."
                            ,mkTooltipQuestionMark ("The representative genome is the best genome in our data in terms of QC stats.")
                            ]
                    )
            , Html.p []
                (let
                    n = mags
                            |> List.filter (\m -> m.taxonomy == mag.taxonomy)
                            |> List.length
                in
                    [if n == 1
                        then Html.text ""
                        else Html.a [HtmlAttr.href ("/genomes?taxnav=1&taxonomy="++mag.taxonomy)]
                                [Html.text <|
                                        "A total of " ++ String.fromInt n ++
                                            " genomes of "++ printableTaxonomy mag.taxonomy ++ " are available (click to see all)"
                                ]])
            ]]
    , Grid.simpleRow [ Grid.col []
        [ Html.h2 []
           [ Html.text "Antibiotic resistance genes" ]
           ,showARGs model mag
           , Html.p []
                [ Html.text "ARGs are predicted using RGI (Resistance Gene Identifier) based on the "
                , Html.a [ HtmlAttr.href "https://card.mcmaster.ca/" ] [ Html.text "Comprehensive Antibiotic Resistance Database (CARD)" ]
                , Html.text ". Results were mapped to "
                , Html.a [ HtmlAttr.href "https://genepi.food.dtu.dk/resfinder"]
                        [ Html.text "ResFinder" ]
                , Html.text " using "
                , Html.a [ HtmlAttr.href "https://doi.org/10.1093/bioinformatics/btaf173" ]
                        [ Html.text "argNorm" ]
                , Html.text "."
                ]
        ]]
    , Grid.simpleRow [ Grid.col []
        [ Html.h2 []
            [ Html.text "Genome organisation" ]
        , showGenomeMap model
        ]]
    , Grid.simpleRow [ Grid.col []
        [ Html.h2 []
            [ Html.text "Download data" ]
        , Html.ol []
            [ Html.li []
                [ Html.a
                    [HtmlAttr.href (mkFASTALink mag.id)]
                    [Html.text "FASTA file (sequence)" ]
                ]
            , Html.li []
                [ Html.text "RGI predictions (antibiotic resistance genes)" ]
            ]
        ]]
    ]


microbeAtlasBaseURL : String
microbeAtlasBaseURL =
    "https://microbeatlas.org/taxon?taxon_id="

showSingle16s : Model -> Int -> MicrobeAtlasMatch -> Html.Html Msg
showSingle16s model ix m =
    Html.div [ HtmlAttr.style "padding-left" "1em"
                , HtmlAttr.style "margin-bottom" "1em"
                , HtmlAttr.style "border-left" "2px solid black"
                , HtmlAttr.style "position" "relative" -- create a containing block for the absolute positioned index later
                ]
            [ if List.member ix model.expanded16S
                then
                    Html.p [HtmlAttr.class "sequence"]
                        [ Html.text m.seq]
                else
                    Html.p [HtmlAttr.class "sequence", HE.onClick (Expand16S ix)]
                        [ Html.text <| String.slice 0 60 m.seq ++ "..." ]
            , Html.p []
                [Html.text "Maps to microbe atlas OTU: "
                , Html.a [ HtmlAttr.href (microbeAtlasBaseURL ++ m.otu), HtmlAttr.class "microbeAtlasLink" ]
                    [ Html.text m.otu ]
                ]
            , Html.div
                [ HtmlAttr.style "position" "absolute"
                , HtmlAttr.style "left" "0px"
                , HtmlAttr.style "transform" "translateX(-120%)"
                , HtmlAttr.style "top" "0px"
                , HtmlAttr.style "background-color" "#333"
                , HtmlAttr.style "color" "#ccc"
                , HtmlAttr.style "padding" "0.2em 0.5em"
                ]
                [ Html.text (String.fromInt (ix + 1))
                ]
            ]

show16S : Model -> MAG -> List (Table.Row Msg)
show16S model mag =
    case model.magdata of
        Waiting ->
            [basicTR "waiting for data..." ""]
        LoadError e ->
            [basicTR "Data load error" e]
        Loaded magdata ->
            if model.expanded
                then
                [ Table.tr []
                    [ Table.td []
                        [ Html.h5 [HtmlAttr.style "color" "black"]
                            [ Html.text "16s rRNA" ]
                        , Html.p [HtmlAttr.style "padding-top" "3em"
                                , HtmlAttr.style "margin-right" "1.5em"
                                , HtmlAttr.style "font-style" "italic"
                                ]
                            [ Html.text
                                """Microbe Atlas (by the von Mering group) is a
                                database of 16S amplicon sequences and where
                                they are found in the environment. For details,
                                see """
                            , Html.a [HtmlAttr.href "https://doi.org/10.1016/j.cell.2026.01.021"]
                                [ Html.text "(Matias Rodrigues et al., 2026)"]
                            , Html.text "."
                            ]

                        , Html.p [HtmlAttr.style "margin-right" "1.5em"
                                , HtmlAttr.style "font-style" "italic"
                                ]
                            [ Html.text
                                """Following the links on the right will take you to the
                                Microbe Atlas database where you can find information about
                                where these sequences are found in other environments."""
                            ]
                        ]
                    , Table.td []
                        [ Html.div []
                            (magdata.microbeAtlas
                                |> List.indexedMap (showSingle16s model)
                            )
                        ]
                    ]
                ]
                else
                [ Table.tr [Table.rowAttr <| HE.onClick Toggle16SExpanded]
                    [ Table.td [ ]
                        [ Html.span [HE.onClick Toggle16SExpanded ]
                            [ Html.text "#16s rRNA "
                            , Html.a [HtmlAttr.href "#"
                                     ,HE.onClick Toggle16SExpanded]
                                [Html.text "(click to see details & matches)" ]
                            ]
                        ]
                    , Table.td []
                        [ Html.text <| String.fromInt mag.r16sRrna ++ " matches" ]
                    ]
                ]

showARGs : Model -> MAG -> Html.Html Msg
showARGs model mag =
    case model.magdata of
        Waiting ->
            Html.p [] [Html.text "Waiting for data..."]
        LoadError e ->
            Html.p [] [Html.text <| "Data load error: " ++ e]
        Loaded magdata ->
            if List.isEmpty magdata.argData then
                Html.p [] [Html.text "No ARGs found for this genome."]
            else
                let
                    hasLowThreshold = List.any (\arg -> arg.matchID < 80) magdata.argData
                in
                Html.div []
                    [ Table.table
                        { options = [ Table.striped, Table.hover, Table.responsive ]
                        , thead = Table.simpleThead
                                            [ Table.th []
                                                [ Html.text "Sequence"
                                                , Html.span [HE.onClick ToggleShowARGSequences]
                                                    [ Html.text <| (if model.showARGSequences then " (collapse)" else " (expand)")
                                                    ]
                                                ]
                                            , Table.th []
                                                [ Html.text "ARG Name" ]
                                            , Table.th []
                                                [ Html.text "Match category (RGI)" ]
                                            , Table.th []
                                                [ Html.text "Match ID (%)" ]
                                            , Table.th []
                                                [ Html.text "Fraction Match (%)" ]
                                            , Table.th []
                                                [ Html.text "Drug Class(es)" ]
                                            , Table.th []
                                                [ Html.text "In ResFinder?"
                                                , mkTooltipQuestionMark
                                                    ("The ResFinder database focuses on clinically relevant ARGs.")
                                                ]
                                            ]
                    , tbody = Table.tbody []
                            (magdata.argData
                                |> List.map (\arg ->
                                    let
                                        lowThreshold = arg.matchID < 80
                                        greyStyle = if lowThreshold
                                            then [ Table.rowAttr <| HtmlAttr.style "color" "#555"
                                                 ]
                                            else []
                                    in
                                        Table.tr greyStyle
                                            [Table.td [] [Html.p [HtmlAttr.class "sequence"]
                                                [Html.text <| (if model.showARGSequences then arg.seq else String.slice 0 30 arg.seq ++ "...")]]
                                            ,Table.td [] [Html.text <| arg.argName ++ " "
                                                         ,Html.a [HtmlAttr.href <| "https://card.mcmaster.ca/" ++ arg.aro]
                                                            [Html.text <| "(" ++ arg.aro ++ ")"]]
                                            ,Table.td [] [Html.text arg.cutOff]
                                            ,Table.td [] [Html.text <| String.fromFloat arg.matchID]
                                            ,Table.td [] [Html.text <| String.fromFloat arg.fractionMatch]
                                            ,Table.td [] [Html.text arg.drugClass]
                                            ,Table.td [] [Html.text (if arg.inResfinder then "✔" else "✘")]
                                            ])
                                )
                    }
                    , if hasLowThreshold then
                        Html.p [HtmlAttr.style "font-size" "small", HtmlAttr.style "color" "#555"]
                            [Html.text "Greyed-out hits (below 80% identity) are shown for completeness but were not used in the analyses."]
                      else
                        Html.text ""
                    ]

-- EMAPPER TSV PARSING

parseEMapperTsv : String -> Result String (List EMapperGene)
parseEMapperTsv tsv =
    let
        lines = String.lines tsv
            |> List.filter (\l -> not (String.isEmpty (String.trim l)))
    in
    case lines of
        [] -> Err "Empty TSV"
        _ :: dataLines ->
            let
                parsed = List.filterMap parseTsvLine dataLines
            in
            if List.isEmpty parsed then
                Err "No valid data rows found"
            else
                Ok parsed


parseTsvLine : String -> Maybe EMapperGene
parseTsvLine line =
    let
        splitCommas s =
            if s == "-" then []
            else String.split "," s
    in
    case String.split "\t" line of
        seqid :: contig :: startStr :: endStr :: strand :: cogCat :: prefName :: rest ->
            case ( String.toInt startStr, String.toInt endStr ) of
                ( Just start, Just end ) ->
                    let
                        keggKo = case rest of
                            ko :: _ -> splitCommas ko
                            _ -> []
                        keggModule = case rest of
                            _ :: km :: _ -> splitCommas km
                            _ -> []
                    in
                    Just
                        { seqid = seqid
                        , contig = contig
                        , start = start
                        , end = end
                        , strand = strand
                        , cogCategory = if cogCat == "-" then "" else cogCat
                        , preferredName = if prefName == "-" then "" else prefName
                        , keggKo = keggKo
                        , keggModule = keggModule
                        }
                _ -> Nothing
        _ -> Nothing


-- GENOME MAP VISUALIZATION

selectedGeneFromState : GeneSequenceState -> Maybe EMapperGene
selectedGeneFromState state =
    case state of
        NoGeneSelected -> Nothing
        GeneSequenceLoading gene -> Just gene
        GeneSequenceLoaded gene _ -> Just gene
        GeneSequenceError gene _ -> Just gene


showGenomeMap : Model -> Html.Html Msg
showGenomeMap model =
    case model.emapperData of
        EMapperWaiting ->
            Html.p [] [ Html.text "Loading genome map..." ]
        EMapperError e ->
            Html.p [] [ Html.text ("Could not load genome map: " ++ e) ]
        EMapperLoaded genes ->
            let
                contigs = groupByContig genes
                globalMax = contigs
                    |> List.map .maxPos
                    |> List.maximum
                    |> Maybe.withDefault 1
                selectedGene = selectedGeneFromState model.geneSequence
            in
            Html.div []
                [ genomeMapControls model globalMax
                , renderGenomeMap model.genomeMapZoom model.genomeMapOffset selectedGene genes
                , renderGeneDetail model.showDnaSequence model.geneSequence
                , renderCogLegend genes
                , Html.div [ HtmlAttr.style "margin-top" "1em" ]
                    [ Html.button
                        [ HE.onClick ToggleGeneTable
                        , HtmlAttr.class "btn btn-outline-secondary btn-sm"
                        ]
                        [ Html.text
                            (if model.showGeneTable
                                then "Hide gene table"
                                else "Show all genes"
                            )
                        ]
                    ]
                , if model.showGeneTable
                    then renderGeneTable genes
                    else Html.text ""
                , Html.p [ HtmlAttr.style "margin-top" "1em"
                         , HtmlAttr.style "font-size" "0.9em"
                         , HtmlAttr.style "color" "#666"
                         ]
                    [ Html.text "Gene predictions and functional annotations were generated using "
                    , Html.a [ HtmlAttr.href "https://github.com/eggnogdb/eggnog-mapper"
                             , HtmlAttr.target "_blank"
                             ]
                        [ Html.text "eggNOG-mapper" ]
                    , Html.text " ("
                    , Html.a [ HtmlAttr.href "https://doi.org/10.1093/molbev/msab293"
                             , HtmlAttr.target "_blank"
                             ]
                        [ Html.text "Cantalapiedra et al., 2021" ]
                    , Html.text ")."
                    ]
                ]


genomeMapControls : Model -> Int -> Html.Html Msg
genomeMapControls model globalMax =
    let
        zoom = model.genomeMapZoom
        offset = model.genomeMapOffset
        viewFraction = 1 / zoom
        viewStartFrac = offset * (1 - viewFraction)
        viewStartBp = round (viewStartFrac * toFloat globalMax)
        viewEndBp = Basics.min globalMax (round ((viewStartFrac + viewFraction) * toFloat globalMax))
        posLabel =
            if zoom > 1 then
                showWithCommas viewStartBp ++ " - " ++ showWithCommas viewEndBp ++ " bp"
            else
                ""
    in
    Html.div
        [ HtmlAttr.style "display" "flex"
        , HtmlAttr.style "align-items" "center"
        , HtmlAttr.style "gap" "0.25em"
        , HtmlAttr.style "margin-bottom" "0.5em"
        ]
        [ Html.div
            [ HtmlAttr.style "display" "inline-flex"
            , HtmlAttr.style "border" "1px solid #ccc"
            , HtmlAttr.style "border-radius" "4px"
            , HtmlAttr.style "overflow" "hidden"
            ]
            [ mapButtonDual GenomeMapScrollLeft (zoom > 1 && offset > 0) "\u{25C0}" "\u{2039}"
            , mapButtonSep
            , mapButton GenomeMapZoomOut (zoom > 1) "\u{2212}"
            , mapButtonSep
            , Html.span
                [ HtmlAttr.style "padding" "0.3em 0.6em"
                , HtmlAttr.style "font-size" "0.85em"
                , HtmlAttr.style "color" "#555"
                , HtmlAttr.style "background" "#fafafa"
                , HtmlAttr.style "min-width" "3em"
                , HtmlAttr.style "text-align" "center"
                , HtmlAttr.style "user-select" "none"
                ]
                [ Html.text (String.fromInt (round zoom) ++ "x") ]
            , mapButtonSep
            , mapButton GenomeMapZoomIn (zoom < 50) "+"
            , mapButtonSep
            , mapButtonDual GenomeMapScrollRight (zoom > 1) "\u{25B6}" "\u{203A}"
            ]
        , if zoom /= 1.0 then
            Html.button
                [ HE.onClick GenomeMapZoomReset
                , HtmlAttr.style "padding" "0.3em 0.7em"
                , HtmlAttr.style "font-size" "0.85em"
                , HtmlAttr.style "cursor" "pointer"
                , HtmlAttr.style "border" "1px solid #ccc"
                , HtmlAttr.style "border-radius" "4px"
                , HtmlAttr.style "background" "#fff"
                , HtmlAttr.style "color" "#555"
                , HtmlAttr.style "margin-left" "0.25em"
                ]
                [ Html.text "Reset" ]
          else
            Html.text ""
        , if String.isEmpty posLabel then
            Html.text ""
          else
            Html.span
                [ HtmlAttr.style "font-size" "0.85em"
                , HtmlAttr.style "color" "#666"
                , HtmlAttr.style "margin-left" "0.5em"
                ]
                [ Html.text posLabel ]
        ]


mapButton : Msg -> Bool -> String -> Html.Html Msg
mapButton msg enabled label =
    Html.button
        [ HE.onClick msg
        , HtmlAttr.disabled (not enabled)
        , HtmlAttr.style "padding" "0.3em 0.7em"
        , HtmlAttr.style "font-size" "0.9em"
        , HtmlAttr.style "cursor" (if enabled then "pointer" else "default")
        , HtmlAttr.style "border" "none"
        , HtmlAttr.style "background" (if enabled then "#fff" else "#f5f5f5")
        , HtmlAttr.style "color" (if enabled then "#333" else "#bbb")
        , HtmlAttr.style "min-width" "2em"
        , HtmlAttr.style "transition" "background 0.15s"
        ]
        [ Html.text label ]


mapButtonDual : Msg -> Bool -> String -> String -> Html.Html Msg
mapButtonDual msg enabled enabledLabel disabledLabel =
    Html.button
        [ HE.onClick msg
        , HtmlAttr.disabled (not enabled)
        , HtmlAttr.style "padding" "0.3em 0.7em"
        , HtmlAttr.style "font-size" "0.9em"
        , HtmlAttr.style "cursor" (if enabled then "pointer" else "default")
        , HtmlAttr.style "border" "none"
        , HtmlAttr.style "background" (if enabled then "#fff" else "#f5f5f5")
        , HtmlAttr.style "color" (if enabled then "#333" else "#bbb")
        , HtmlAttr.style "min-width" "2em"
        , HtmlAttr.style "transition" "background 0.15s"
        ]
        [ Html.text (if enabled then enabledLabel else disabledLabel) ]


mapButtonSep : Html.Html msg
mapButtonSep =
    Html.div
        [ HtmlAttr.style "width" "1px"
        , HtmlAttr.style "background" "#ddd"
        , HtmlAttr.style "align-self" "stretch"
        ]
        []


type alias ContigInfo =
    { name : String
    , maxPos : Int
    , genes : List EMapperGene
    }


groupByContig : List EMapperGene -> List ContigInfo
groupByContig genes =
    let
        contigNames =
            genes
                |> List.map .contig
                |> List.foldl (\c acc -> if List.member c acc then acc else acc ++ [c]) []
    in
    contigNames
        |> List.map (\cname ->
            let
                cGenes = List.filter (\g -> g.contig == cname) genes
                maxP = cGenes
                    |> List.map .end
                    |> List.maximum
                    |> Maybe.withDefault 0
            in
            { name = cname, maxPos = maxP, genes = cGenes }
        )


genomeMapLayout :
    { labelWidth : Float
    , mapWidth : Float
    , rowHeight : Int
    , rowSpacing : Int
    }
genomeMapLayout =
    { labelWidth = 120
    , mapWidth = 970
    , rowHeight = 40
    , rowSpacing = 8
    }


renderGenomeMap : Float -> Float -> Maybe EMapperGene -> List EMapperGene -> Html.Html Msg
renderGenomeMap zoom offset selectedGene genes =
    let
        lay = genomeMapLayout
        contigs = groupByContig genes
        svgHeight = List.length contigs * (lay.rowHeight + lay.rowSpacing) + 10
        globalMax = contigs
            |> List.map .maxPos
            |> List.maximum
            |> Maybe.withDefault 1
        -- Virtual width of the full zoomed map
        virtualWidth = lay.mapWidth * zoom
        -- viewBox: we see mapWidth worth of virtual coords, offset into the virtual space
        vbX = offset * (virtualWidth - lay.mapWidth)
        -- Scale maps genome position to virtual x coordinate (0 .. virtualWidth)
        scale pos = toFloat pos / toFloat globalMax * virtualWidth
        -- Show arrows only when the visible region is small enough
        visibleBp = toFloat globalMax / zoom
        useArrows = visibleBp < 500000
    in
    Html.div []
        [ Html.div
            [ HtmlAttr.style "display" "flex"
            , HtmlAttr.style "border" "1px solid #ddd"
            ]
            [ -- Fixed labels column
              Svg.svg
                [ SvgAttr.width (String.fromFloat lay.labelWidth)
                , SvgAttr.height (String.fromInt svgHeight)
                , SvgAttr.viewBox ("0 0 " ++ String.fromFloat lay.labelWidth ++ " " ++ String.fromInt svgHeight)
                , SvgAttr.style "font-family: sans-serif; font-size: 11px; flex-shrink: 0;"
                ]
                (contigs
                    |> List.indexedMap (\i contig ->
                        let
                            y = toFloat (i * (lay.rowHeight + lay.rowSpacing)) + 5
                            cy = y + toFloat lay.rowHeight / 2
                        in
                        Svg.text_
                            [ SvgAttr.x "5"
                            , SvgAttr.y (String.fromFloat (cy + 4))
                            , SvgAttr.fill "#333"
                            ]
                            [ Svg.text contig.name ]
                    )
                )
            , -- Scrollable map area
              Svg.svg
                [ SvgAttr.width (String.fromFloat lay.mapWidth)
                , SvgAttr.height (String.fromInt svgHeight)
                , SvgAttr.viewBox
                    (String.fromFloat vbX
                        ++ " 0 "
                        ++ String.fromFloat lay.mapWidth
                        ++ " " ++ String.fromInt svgHeight
                    )
                , SvgAttr.style "font-family: sans-serif; font-size: 11px; flex-grow: 1;"
                ]
                (contigs
                    |> List.indexedMap (\i contig ->
                        let
                            y = toFloat (i * (lay.rowHeight + lay.rowSpacing)) + 5
                            cy = y + toFloat lay.rowHeight / 2
                        in
                        Svg.g []
                            ([ Svg.line
                                [ SvgAttr.x1 (String.fromFloat (scale 0))
                                , SvgAttr.y1 (String.fromFloat cy)
                                , SvgAttr.x2 (String.fromFloat (scale contig.maxPos))
                                , SvgAttr.y2 (String.fromFloat cy)
                                , SvgAttr.stroke "black"
                                , SvgAttr.strokeWidth "3"
                                ]
                                []
                            ]
                            ++ List.map (\gene -> renderGeneArrow useArrows scale cy selectedGene gene) contig.genes
                            )
                    )
                )
            ]
        , renderOverviewBar zoom offset contigs globalMax
        ]


renderOverviewBar : Float -> Float -> List ContigInfo -> Int -> Html.Html Msg
renderOverviewBar zoom offset contigs globalMax =
    let
        lay = genomeMapLayout
        barWidth = lay.mapWidth
        barHeight = 6 * toFloat (List.length contigs) + 4
        viewFraction = 1 / zoom
        viewStart = offset * (1 - viewFraction)
        rectX = lay.labelWidth + viewStart * barWidth
        rectW = Basics.max 4 (viewFraction * barWidth)
    in
    if zoom <= 1 then
        Html.text ""
    else
        Html.div
            [ HtmlAttr.style "display" "flex" ]
            [ Html.div
                [ HtmlAttr.style "width" (String.fromFloat lay.labelWidth ++ "px")
                , HtmlAttr.style "flex-shrink" "0"
                ]
                []
            , Svg.svg
                [ SvgAttr.width (String.fromFloat barWidth)
                , SvgAttr.height (String.fromFloat barHeight)
                , SvgAttr.style "display: block; margin-top: 2px;"
                ]
                (List.indexedMap
                    (\i contig ->
                        let
                            cy = toFloat i * 6 + 5
                            cWidth = toFloat contig.maxPos / toFloat globalMax * barWidth
                        in
                        Svg.line
                            [ SvgAttr.x1 "0"
                            , SvgAttr.y1 (String.fromFloat cy)
                            , SvgAttr.x2 (String.fromFloat cWidth)
                            , SvgAttr.y2 (String.fromFloat cy)
                            , SvgAttr.stroke "#bbb"
                            , SvgAttr.strokeWidth "3"
                            ]
                            []
                    )
                    contigs
                    ++ [ Svg.rect
                            [ SvgAttr.x (String.fromFloat (viewStart * barWidth))
                            , SvgAttr.y "0"
                            , SvgAttr.width (String.fromFloat rectW)
                            , SvgAttr.height (String.fromFloat barHeight)
                            , SvgAttr.fill "rgba(66, 133, 244, 0.3)"
                            , SvgAttr.stroke "#4285f4"
                            , SvgAttr.strokeWidth "1"
                            , SvgAttr.rx "2"
                            ]
                            []
                       ]
                )
            ]


renderGeneArrow : Bool -> (Int -> Float) -> Float -> Maybe EMapperGene -> EMapperGene -> Svg.Svg Msg
renderGeneArrow useArrows scale cy selectedGene gene =
    let
        x1 = scale gene.start
        x2 = scale gene.end
        geneWidth = x2 - x1
        halfHeight = 8
        color = cogColor (String.left 1 gene.cogCategory)
        tooltipText =
            (if String.isEmpty gene.preferredName then gene.seqid else gene.preferredName)
                ++ " [" ++ (if String.isEmpty gene.cogCategory then "-" else gene.cogCategory) ++ "]"
        isSelected = case selectedGene of
            Just sel -> sel.seqid == gene.seqid
            Nothing -> False
        strokeColor = if isSelected then "#000" else if useArrows then "#333" else color
        strokeW = if isSelected then "2" else if useArrows then "0.5" else "0.3"
        clickHandler = SvgEvents.onClick (GeneClicked gene)
        cursorStyle = SvgAttr.style "cursor: pointer;"
    in
    if not useArrows then
        Svg.rect
            [ SvgAttr.x (String.fromFloat x1)
            , SvgAttr.y (String.fromFloat (cy - halfHeight))
            , SvgAttr.width (String.fromFloat (Basics.max 1 geneWidth))
            , SvgAttr.height (String.fromFloat (halfHeight * 2))
            , SvgAttr.fill color
            , SvgAttr.stroke strokeColor
            , SvgAttr.strokeWidth strokeW
            , clickHandler
            , cursorStyle
            ]
            [ Svg.title [] [ Svg.text tooltipText ]
            ]
    else
        let
            arrowHeadSize = Basics.min 6 (geneWidth * 0.3)
            points =
                if gene.strand == "+" then
                    String.join " "
                        [ p x1 (cy - halfHeight)
                        , p (x2 - arrowHeadSize) (cy - halfHeight)
                        , p x2 cy
                        , p (x2 - arrowHeadSize) (cy + halfHeight)
                        , p x1 (cy + halfHeight)
                        ]
                else
                    String.join " "
                        [ p (x1 + arrowHeadSize) (cy - halfHeight)
                        , p x2 (cy - halfHeight)
                        , p x2 (cy + halfHeight)
                        , p (x1 + arrowHeadSize) (cy + halfHeight)
                        , p x1 cy
                        ]
        in
        Svg.polygon
            [ SvgAttr.points points
            , SvgAttr.fill color
            , SvgAttr.stroke strokeColor
            , SvgAttr.strokeWidth strokeW
            , clickHandler
            , cursorStyle
            ]
            [ Svg.title [] [ Svg.text tooltipText ]
            ]


p : Float -> Float -> String
p x y =
    String.fromFloat x ++ "," ++ String.fromFloat y


cogColor : String -> String
cogColor cat =
    case cat of
        "J" -> "#f06292"   -- Translation
        "A" -> "#e91e63"   -- RNA processing
        "K" -> "#ab47bc"   -- Transcription
        "L" -> "#7e57c2"   -- Replication/repair
        "B" -> "#5c6bc0"   -- Chromatin
        "D" -> "#66bb6a"   -- Cell cycle
        "Y" -> "#a5d6a7"   -- Nuclear structure
        "V" -> "#43a047"   -- Defense
        "T" -> "#ff9800"   -- Signal transduction
        "M" -> "#8bc34a"   -- Cell wall
        "N" -> "#00acc1"   -- Cell motility
        "U" -> "#26c6da"   -- Secretion
        "O" -> "#ec407a"   -- Post-translational modification
        "C" -> "#42a5f5"   -- Energy
        "G" -> "#fdd835"   -- Carbohydrate
        "E" -> "#ffb74d"   -- Amino acid
        "F" -> "#9575cd"   -- Nucleotide
        "H" -> "#64b5f6"   -- Coenzyme
        "I" -> "#a1887f"   -- Lipid
        "P" -> "#4db6ac"   -- Inorganic ion
        "Q" -> "#ff7043"   -- Secondary metabolites
        "R" -> "#bdbdbd"   -- General function
        "S" -> "#e0e0e0"   -- Function unknown
        "" -> "#f5f5f5"    -- No COG
        _ -> "#cccccc"


cogDescription : String -> String
cogDescription cat =
    case cat of
        "J" -> "Translation, ribosomal structure"
        "A" -> "RNA processing and modification"
        "K" -> "Transcription"
        "L" -> "Replication, recombination and repair"
        "B" -> "Chromatin structure"
        "D" -> "Cell cycle control, cell division"
        "Y" -> "Nuclear structure"
        "V" -> "Defense mechanisms"
        "T" -> "Signal transduction"
        "M" -> "Cell wall/membrane biogenesis"
        "N" -> "Cell motility"
        "U" -> "Intracellular trafficking, secretion"
        "O" -> "Post-translational modification"
        "C" -> "Energy production and conversion"
        "G" -> "Carbohydrate transport and metabolism"
        "E" -> "Amino acid transport and metabolism"
        "F" -> "Nucleotide transport and metabolism"
        "H" -> "Coenzyme transport and metabolism"
        "I" -> "Lipid transport and metabolism"
        "P" -> "Inorganic ion transport and metabolism"
        "Q" -> "Secondary metabolites"
        "R" -> "General function prediction only"
        "S" -> "Function unknown"
        "" -> "No COG category"
        _ -> cat


renderGeneDetail : Bool -> GeneSequenceState -> Html.Html Msg
renderGeneDetail showDna state =
    case state of
        NoGeneSelected ->
            Html.text ""

        GeneSequenceLoading gene ->
            Html.div [ HtmlAttr.class "gene-detail-panel" ]
                [ geneDetailHeader gene
                , geneAnnotationInfo gene
                , Html.p [] [ Html.text "Loading sequence..." ]
                ]

        GeneSequenceError gene errMsg ->
            Html.div [ HtmlAttr.class "gene-detail-panel" ]
                [ geneDetailHeader gene
                , geneAnnotationInfo gene
                , Html.p [ HtmlAttr.style "color" "#c00" ]
                    [ Html.text ("Error: " ++ errMsg) ]
                ]

        GeneSequenceLoaded gene seqData ->
            Html.div [ HtmlAttr.class "gene-detail-panel" ]
                [ geneDetailHeader gene
                , geneAnnotationInfo gene
                , Html.div []
                    [ Html.div [ HtmlAttr.style "display" "flex"
                               , HtmlAttr.style "align-items" "baseline"
                               , HtmlAttr.style "gap" "0.5em"
                               ]
                        [ Html.h4 [ HtmlAttr.style "margin" "0.5em 0 0.25em 0" ]
                            [ Html.text "Protein sequence"
                            , Html.span [ HtmlAttr.style "font-weight" "normal"
                                        , HtmlAttr.style "font-size" "0.85em"
                                        , HtmlAttr.style "color" "#666"
                                        , HtmlAttr.style "margin-left" "0.5em"
                                        ]
                                [ Html.text ("(" ++ showWithCommas (String.length seqData.protein) ++ " aa, translation table 11)") ]
                            ]
                        , copyButton "copy-protein" seqData.protein
                        ]
                    , Html.div [ HtmlAttr.class "sequence" ]
                        (coloredProtein seqData.protein)
                    , aaLegend
                    , Html.div [ HtmlAttr.style "display" "flex"
                               , HtmlAttr.style "align-items" "baseline"
                               , HtmlAttr.style "gap" "0.5em"
                               ]
                        [ Html.h4 [ HtmlAttr.style "margin" "0.5em 0 0.25em 0"
                                  , HtmlAttr.style "cursor" "pointer"
                                  , HE.onClick ToggleDnaSequence
                                  ]
                            [ Html.text (if showDna then "\u{25BC} " else "\u{25B6} ")
                            , Html.text "DNA sequence"
                            , Html.span [ HtmlAttr.style "font-weight" "normal"
                                        , HtmlAttr.style "font-size" "0.85em"
                                        , HtmlAttr.style "color" "#666"
                                        , HtmlAttr.style "margin-left" "0.5em"
                                        ]
                                [ Html.text ("(" ++ showWithCommas (String.length seqData.dna) ++ " bp)") ]
                            ]
                        , if showDna then copyButton "copy-dna" seqData.dna else Html.text ""
                        ]
                    , if showDna then
                        Html.p [ HtmlAttr.class "sequence" ]
                            [ Html.text seqData.dna ]
                      else
                        Html.text ""
                    ]
                ]


aaGroupColor : Char -> String
aaGroupColor aa =
    case aa of
        -- Nonpolar/hydrophobic
        'G' -> "#f9e79f"
        'A' -> "#f9e79f"
        'V' -> "#f9e79f"
        'L' -> "#f9e79f"
        'I' -> "#f9e79f"
        'P' -> "#f9e79f"
        'F' -> "#f9e79f"
        'M' -> "#f9e79f"
        'W' -> "#f9e79f"
        -- Polar/uncharged
        'S' -> "#abebc6"
        'T' -> "#abebc6"
        'C' -> "#abebc6"
        'Y' -> "#abebc6"
        'N' -> "#abebc6"
        'Q' -> "#abebc6"
        -- Positively charged
        'K' -> "#aed6f1"
        'R' -> "#aed6f1"
        'H' -> "#aed6f1"
        -- Negatively charged
        'D' -> "#f5b7b1"
        'E' -> "#f5b7b1"
        -- Stop
        '*' -> "#d5d8dc"
        _ -> "transparent"


coloredProtein : String -> List (Html.Html msg)
coloredProtein protein =
    protein
        |> String.toList
        |> List.map (\aa ->
            Html.span
                [ HtmlAttr.style "background-color" (aaGroupColor aa) ]
                [ Html.text (String.fromChar aa) ]
        )


aaLegend : Html.Html msg
aaLegend =
    Html.div
        [ HtmlAttr.style "display" "flex"
        , HtmlAttr.style "flex-wrap" "wrap"
        , HtmlAttr.style "gap" "0.3em 1em"
        , HtmlAttr.style "font-size" "0.8em"
        , HtmlAttr.style "margin-top" "0.3em"
        ]
        (List.map aaLegendItem
            [ ("#f9e79f", "Nonpolar (G, A, V, L, I, P, F, M, W)")
            , ("#abebc6", "Polar (S, T, C, Y, N, Q)")
            , ("#aed6f1", "Positive (K, R, H)")
            , ("#f5b7b1", "Negative (D, E)")
            , ("#d5d8dc", "Stop (*)")
            ]
        )


aaLegendItem : (String, String) -> Html.Html msg
aaLegendItem (color, label) =
    Html.span
        [ HtmlAttr.style "display" "inline-flex"
        , HtmlAttr.style "align-items" "center"
        , HtmlAttr.style "gap" "0.3em"
        ]
        [ Html.span
            [ HtmlAttr.style "display" "inline-block"
            , HtmlAttr.style "width" "14px"
            , HtmlAttr.style "height" "14px"
            , HtmlAttr.style "background-color" color
            , HtmlAttr.style "border" "1px solid #999"
            ]
            []
        , Html.text label
        ]


geneAnnotationInfo : EMapperGene -> Html.Html Msg
geneAnnotationInfo gene =
    let
        row label content =
            Html.tr []
                [ Html.td [ HtmlAttr.style "padding" "0.15em 0.75em 0.15em 0"
                           , HtmlAttr.style "color" "#666"
                           , HtmlAttr.style "vertical-align" "top"
                           , HtmlAttr.style "white-space" "nowrap"
                           ]
                    [ Html.text label ]
                , Html.td [ HtmlAttr.style "padding" "0.15em 0" ]
                    content
                ]
        cogRow =
            if String.isEmpty gene.cogCategory then
                []
            else
                [ row "COG category"
                    [ Html.text (gene.cogCategory ++ " — " ++ cogDescription (String.left 1 gene.cogCategory)) ]
                ]
        keggKoRow =
            if List.isEmpty gene.keggKo then
                []
            else
                [ row "KEGG orthology"
                    (List.intersperse (Html.text ", ")
                        (List.map keggKoLink gene.keggKo))
                ]
        keggModuleRow =
            if List.isEmpty gene.keggModule then
                []
            else
                [ row "KEGG module"
                    (List.intersperse (Html.text ", ")
                        (List.map keggModuleLink gene.keggModule))
                ]
        rows = cogRow ++ keggKoRow ++ keggModuleRow
    in
    if List.isEmpty rows then
        Html.text ""
    else
        Html.table [ HtmlAttr.style "margin" "0.5em 0"
                   , HtmlAttr.style "font-size" "0.9em"
                   ]
            [ Html.tbody [] rows ]


keggKoLink : String -> Html.Html msg
keggKoLink ko =
    let
        koId = String.replace "ko:" "" ko
    in
    Html.a [ HtmlAttr.href ("https://www.genome.jp/dbget-bin/www_bget?" ++ koId)
           , HtmlAttr.target "_blank"
           ]
        [ Html.text ko ]


keggModuleLink : String -> Html.Html msg
keggModuleLink m =
    Html.a [ HtmlAttr.href ("https://www.genome.jp/dbget-bin/www_bget?" ++ m)
           , HtmlAttr.target "_blank"
           ]
        [ Html.text m ]


copyButton : String -> String -> Html.Html Msg
copyButton buttonId text =
    Html.button
        [ HtmlAttr.id buttonId
        , HE.onClick (CopyToClipboard text buttonId)
        , HtmlAttr.class "copy-btn"
        ]
        [ Html.text "\u{1F4CB}" ]


geneDetailHeader : EMapperGene -> Html.Html Msg
geneDetailHeader gene =
    Html.div [ HtmlAttr.style "display" "flex"
             , HtmlAttr.style "justify-content" "space-between"
             , HtmlAttr.style "align-items" "center"
             ]
        [ Html.h3 [ HtmlAttr.style "margin" "0" ]
            [ Html.text
                (if String.isEmpty gene.preferredName
                    then gene.seqid
                    else gene.preferredName ++ " (" ++ gene.seqid ++ ")"
                )
            ]
        , Html.span [ HtmlAttr.style "font-size" "0.85em"
                    , HtmlAttr.style "color" "#666"
                    , HtmlAttr.style "margin-left" "1em"
                    ]
            [ Html.text (gene.contig
                ++ " : " ++ showWithCommas gene.start
                ++ "-" ++ showWithCommas gene.end
                ++ " [" ++ gene.strand ++ "]"
                )
            ]
        , Html.button
            [ HE.onClick CloseGeneDetail
            , HtmlAttr.style "border" "none"
            , HtmlAttr.style "background" "none"
            , HtmlAttr.style "font-size" "1.2em"
            , HtmlAttr.style "cursor" "pointer"
            , HtmlAttr.style "color" "#666"
            , HtmlAttr.style "padding" "0.2em 0.5em"
            ]
            [ Html.text "\u{2715}" ]
        ]


renderGeneTable : List EMapperGene -> Html.Html Msg
renderGeneTable genes =
    Html.div [ HtmlAttr.style "margin-top" "1em"
             , HtmlAttr.style "max-height" "600px"
             , HtmlAttr.style "overflow-y" "auto"
             ]
        [ Html.table [ HtmlAttr.class "table table-sm table-striped"
                     , HtmlAttr.style "font-size" "0.85em"
                     ]
            [ Html.thead []
                [ Html.tr []
                    [ Html.th [] [ Html.text "Gene ID" ]
                    , Html.th [] [ Html.text "Contig" ]
                    , Html.th [] [ Html.text "Start" ]
                    , Html.th [] [ Html.text "End" ]
                    , Html.th [] [ Html.text "Strand" ]
                    , Html.th [] [ Html.text "Size (bp)" ]
                    , Html.th [] [ Html.text "Gene name" ]
                    , Html.th [] [ Html.text "COG" ]
                    , Html.th [] [ Html.text "KEGG KO" ]
                    , Html.th [] [ Html.text "KEGG Module" ]
                    ]
                ]
            , Html.tbody []
                (genes |> List.map (\gene ->
                    Html.tr []
                        [ Html.td [] [ Html.text gene.seqid ]
                        , Html.td [] [ Html.text gene.contig ]
                        , Html.td [ HtmlAttr.style "text-align" "right" ] [ Html.text (showWithCommas gene.start) ]
                        , Html.td [ HtmlAttr.style "text-align" "right" ] [ Html.text (showWithCommas gene.end) ]
                        , Html.td [] [ Html.text gene.strand ]
                        , Html.td [ HtmlAttr.style "text-align" "right" ] [ Html.text (showWithCommas (gene.end - gene.start + 1)) ]
                        , Html.td []
                            [ Html.text
                                (if String.isEmpty gene.preferredName
                                    then "-"
                                    else gene.preferredName
                                )
                            ]
                        , Html.td []
                            [ Html.span [ HtmlAttr.style "color" (cogColor (String.left 1 gene.cogCategory)) ]
                                [ Html.text
                                    (if String.isEmpty gene.cogCategory
                                        then "-"
                                        else gene.cogCategory
                                    )
                                ]
                            ]
                        , Html.td []
                            (if List.isEmpty gene.keggKo
                                then [ Html.text "-" ]
                                else gene.keggKo
                                    |> List.map keggKoLink
                                    |> List.intersperse (Html.text ", ")
                            )
                        , Html.td []
                            (if List.isEmpty gene.keggModule
                                then [ Html.text "-" ]
                                else gene.keggModule
                                    |> List.map keggModuleLink
                                    |> List.intersperse (Html.text ", ")
                            )
                        ]
                ))
            ]
        ]


renderCogLegend : List EMapperGene -> Html.Html Msg
renderCogLegend genes =
    let
        usedCats =
            genes
                |> List.map (\g -> String.left 1 g.cogCategory)
                |> List.foldl (\c acc -> if List.member c acc then acc else acc ++ [c]) []
                |> List.sort
        allCats = ["J","A","K","L","B","D","Y","V","T","M","N","U","O","C","G","E","F","H","I","P","Q","R","S",""]
        catsToShow = allCats |> List.filter (\c -> List.member c usedCats)
    in
    Html.div [ HtmlAttr.style "margin-top" "0.5em"
             , HtmlAttr.style "display" "flex"
             , HtmlAttr.style "flex-wrap" "wrap"
             , HtmlAttr.style "gap" "0.3em 1em"
             , HtmlAttr.style "font-size" "0.85em"
             ]
        (catsToShow |> List.map (\cat ->
            Html.span [ HtmlAttr.style "display" "inline-flex"
                      , HtmlAttr.style "align-items" "center"
                      , HtmlAttr.style "gap" "0.3em"
                      ]
                [ Html.span
                    [ HtmlAttr.style "display" "inline-block"
                    , HtmlAttr.style "width" "14px"
                    , HtmlAttr.style "height" "14px"
                    , HtmlAttr.style "background-color" (cogColor cat)
                    , HtmlAttr.style "border" "1px solid #999"
                    ]
                    []
                , Html.text ((if String.isEmpty cat then "-" else cat) ++ ": " ++ cogDescription cat)
                ]
        ))
