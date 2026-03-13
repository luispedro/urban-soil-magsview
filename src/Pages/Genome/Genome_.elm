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

import Svg
import Svg.Attributes as SvgAttr

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
    }

type LoadedDataModel =
    Loaded MAGData
    | LoadError String
    | Waiting

type EMapperDataModel
    = EMapperLoaded (List EMapperGene)
    | EMapperError String
    | EMapperWaiting

type alias Model =
    { magdata : LoadedDataModel
    , emapperData : EMapperDataModel
    , expanded : Bool
    , expanded16S : List Int
    , showARGSequences : Bool
    , genomeMapZoom : Float
    , genomeMapOffset : Float
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

model0 : Model
model0 =
    { magdata = Waiting
    , emapperData = EMapperWaiting
    , expanded = False
    , expanded16S = []
    , showARGSequences = False
    , genomeMapZoom = 1.0
    , genomeMapOffset = 0.0
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
        { init = \_ -> (model0, cmd0 route.params.genome)
        , update = update
        , subscriptions = \_ -> Sub.none
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
    case String.split "\t" line of
        seqid :: contig :: startStr :: endStr :: strand :: cogCat :: prefName :: _ ->
            case ( String.toInt startStr, String.toInt endStr ) of
                ( Just start, Just end ) ->
                    Just
                        { seqid = seqid
                        , contig = contig
                        , start = start
                        , end = end
                        , strand = strand
                        , cogCategory = if cogCat == "-" then "" else cogCat
                        , preferredName = if prefName == "-" then "" else prefName
                        }
                _ -> Nothing
        _ -> Nothing


-- GENOME MAP VISUALIZATION

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
            in
            Html.div []
                [ genomeMapControls model globalMax
                , renderGenomeMap model.genomeMapZoom model.genomeMapOffset genes
                , renderCogLegend genes
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


renderGenomeMap : Float -> Float -> List EMapperGene -> Html.Html Msg
renderGenomeMap zoom offset genes =
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
                            ++ List.map (\gene -> renderGeneArrow scale cy gene) contig.genes
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


renderGeneArrow : (Int -> Float) -> Float -> EMapperGene -> Svg.Svg Msg
renderGeneArrow scale cy gene =
    let
        x1 = scale gene.start
        x2 = scale gene.end
        geneWidth = x2 - x1
        arrowHeadSize = Basics.min 6 (geneWidth * 0.3)
        halfHeight = 8
        color = cogColor (String.left 1 gene.cogCategory)
        tooltipText =
            (if String.isEmpty gene.preferredName then gene.seqid else gene.preferredName)
                ++ " [" ++ (if String.isEmpty gene.cogCategory then "-" else gene.cogCategory) ++ "]"
        points =
            if gene.strand == "+" then
                -- Arrow pointing right
                String.join " "
                    [ p x1 (cy - halfHeight)
                    , p (x2 - arrowHeadSize) (cy - halfHeight)
                    , p x2 cy
                    , p (x2 - arrowHeadSize) (cy + halfHeight)
                    , p x1 (cy + halfHeight)
                    ]
            else
                -- Arrow pointing left
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
        , SvgAttr.stroke "#333"
        , SvgAttr.strokeWidth "0.5"
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
