module Pages.Genomes exposing (page, Model, Msg)

import Bytes exposing (Bytes)
import Bytes.Encode
import Dict exposing (Dict)
import File.Download as Download
import Html
import Html.Attributes as HtmlAttr
import Html.Events as HE
import Http
import Time
import Zip
import Zip.Entry

import Route exposing (Route)
import Page exposing (Page)
import View exposing (View)

import Svg as S
import Svg.Attributes as SA
import Svg.Events as SE

import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Chart.Item as CI


import Layouts
import Effect exposing (Effect)
import View exposing (View)

import W.InputCheckbox as InputCheckbox
import W.InputSlider as InputSlider
import W.Modal as Modal
import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Table as Table

import DataModel exposing (MAG)
import Data exposing (mags)
import Data.Info exposing (mkFASTALink, mkReadmeFile, datasetName, datasetTag, datasetSlug)
import GenomeStats exposing (Quality(..), magQuality, taxonomyLast, splitTaxon, showTaxon)
import Shared



type SortOrder =
    ById
    | ByTaxonomy
    | ByCompleteness
    | ByContamination
    | ByNrContigs
    | ByGenomeSize

type DownloadState
    = NotStarted
    | Downloading { expected : Int, fetched : Dict String Bytes }
    | DownloadError String

type alias Model =
    { qualityFilter : Maybe String
    , sortOrder : SortOrder
    , repsOnly : Bool
    , hqOnly : Bool
    , taxonomyFilter : String
    , taxonomyUpActive : Bool
    , maxNrContigsStep : Float

    , showFullTaxonomy : Bool

    , hovering : List (CI.One MAG CI.Dot)

    , showDownloadModal : Bool
    , downloadState : DownloadState
    , useWget : Bool
    }

type Msg =
    SetSortOrder SortOrder
    | DownloadTSV
    | SetRepsOnly Bool
    | SetHqOnly Bool
    | UpdateTaxonomyFilter String
    | UpdateMaxNrContigs Float
    | ToggleShowFullTaxonomy
    | OnHover (List (CI.One MAG CI.Dot))
    | ShowDownloadModal
    | ClearDownload
    | TriggerBulkDownload
    | GotFastaBytes String (Result Http.Error Bytes)
    | DownloadScript String String
    | SetUseWget Bool





init : Route () -> () -> (Model, Effect Msg)
init route () =
    let
        model0 =
            { qualityFilter = Nothing
            , sortOrder = ById
            , repsOnly = False
            , hqOnly = False
            , taxonomyFilter = ""
            , taxonomyUpActive = False
            , maxNrContigsStep = 6
            , showFullTaxonomy = False
            , hovering = []
            , showDownloadModal = False
            , downloadState = NotStarted
            , useWget = True
            }
        model1 = case Dict.get "taxonomy" route.query of
            Just taxonomy ->
               { model0
                    | taxonomyFilter = taxonomy
                    , showFullTaxonomy = True
              }
            Nothing -> model0
        model =
            case Dict.get "taxnav" route.query of
                Just "1" ->
                    { model1 | taxonomyUpActive = True }
                _ ->
                    model1
    in
        ( model
        , Effect.none
        )

page : Shared.Model -> Route () -> Page Model Msg
page _ route =
    Page.new
        { init = init route
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
    |> Page.withLayout (\_ -> Layouts.Main {})


step2maxContigs : Float -> Int
step2maxContigs step =
    if step < 0.5
        then 1
    else if step < 1.5
        then 5
    else if step < 2.5
        then 10
    else if step < 3.5
        then 20
    else if step < 4.5
        then 50
    else if step < 5.5
        then 100
    else -1

update :
    Msg
    -> Model
    -> (Model, Effect Msg)
update msg model =
    case msg of
        SetSortOrder order ->
            ({ model | sortOrder = order }
            , Effect.none)
        SetRepsOnly ro ->
            ({ model | repsOnly = ro }
            , Effect.none)
        SetHqOnly hq ->
            ({ model | hqOnly = hq }
            , Effect.none)
        UpdateTaxonomyFilter filter ->
            ({ model | taxonomyFilter = filter
                    , taxonomyUpActive = upOneLevel filter /= filter
                    }
            , Effect.none)
        UpdateMaxNrContigs step ->
            ({ model | maxNrContigsStep = step }
            , Effect.none)
        ToggleShowFullTaxonomy ->
            ({ model | showFullTaxonomy = not model.showFullTaxonomy }
            , Effect.none)
        DownloadTSV ->
            let
                tsv = mkTSV model
            in
                ( model
                , Effect.sendCmd <| Download.string (datasetTag ++ "_selected_genomes.tsv") "text/tab-separated-values" tsv
                )
        OnHover hovering ->
            ({ model | hovering = hovering }, Effect.none)
        ShowDownloadModal ->
            ({ model | showDownloadModal = True, downloadState = NotStarted }, Effect.none)
        ClearDownload ->
            ({ model | showDownloadModal = False, downloadState = NotStarted }, Effect.none)
        TriggerBulkDownload ->
            let
                sel = filteredMags model
            in
            ( { model | downloadState = Downloading { expected = List.length sel, fetched = Dict.empty } }
            , sel
                |> List.map fetchFastaBytes
                |> Cmd.batch
                |> Effect.sendCmd
            )
        DownloadScript filename content ->
            ( model
            , Effect.sendCmd <| Download.string filename "text/x-shellscript" content
            )
        SetUseWget v ->
            ({ model | useWget = v }, Effect.none)
        GotFastaBytes magId result ->
            case model.downloadState of
                Downloading state ->
                    case result of
                        Ok bytes ->
                            let
                                newFetched = Dict.insert magId bytes state.fetched
                            in
                            if Dict.size newFetched == state.expected then
                                ( { model | downloadState = NotStarted, showDownloadModal = False }
                                , buildAndDownloadZip (filteredMags model) newFetched
                                    |> Effect.sendCmd
                                )
                            else
                                ( { model | downloadState = Downloading { state | fetched = newFetched } }
                                , Effect.none
                                )
                        Err _ ->
                            ( { model | downloadState = DownloadError ("Failed to download " ++ magId) }
                            , Effect.none
                            )
                _ ->
                    ( model, Effect.none )


last : List a -> Maybe a
last xs =
    case List.reverse xs of
        [] ->
            Nothing
        x :: _ ->
            Just x

upOneLevel : String -> String
upOneLevel taxonomy =
    case last (String.indexes ";" taxonomy) of
        Nothing ->
            taxonomy
        Just i ->
            if i > 0 then
                String.left i taxonomy
            else
                taxonomy

mkTSV : Model -> String
mkTSV model =
    let
        sel = mags
            |> List.sortBy (\t ->
                case model.sortOrder of
                    ById ->
                        (t.id, 0.0)
                    ByTaxonomy ->
                        (t.taxonomy, 0.0)
                    ByCompleteness ->
                        ("", -t.completeness)
                    ByContamination ->
                        ("", t.contamination)
                    ByNrContigs ->
                        ("", toFloat t.nrContigs)
                    ByGenomeSize ->
                        ("", toFloat <| -t.genomeSize)
            )
            |> (if model.repsOnly
                    then List.filter .isRepresentative
                    else identity)
            |> (if model.hqOnly
                    then List.filter (\t -> magQuality t == High)
                    else identity)
            |> (if String.isEmpty model.taxonomyFilter
                    then identity
                    else List.filter (\t ->
                            String.contains
                                (String.toLower model.taxonomyFilter)
                                (String.toLower t.taxonomy))
                )
            |> (if step2maxContigs model.maxNrContigsStep > 0
                    then List.filter (\t -> t.nrContigs <= step2maxContigs model.maxNrContigsStep)
                    else identity)

        header =
            [ "MAG ID"
            , "Completeness"
            , "Contamination"
            , "# Contigs"
            , "Genome size (bp)"
            , "# r16s rRNA"
            , "# r5s rRNA"
            , "# r23s rRNA"
            , "# tRNA"
            , "Is representative"
            , "Taxonomy (GTDB)"
            ]

        rows =
            sel
                |> List.map (\t ->
                    [ t.id
                    , String.fromFloat t.completeness
                    , String.fromFloat t.contamination
                    , String.fromInt t.nrContigs
                    , String.fromFloat <| toFloat t.genomeSize
                    , String.fromInt t.r16sRrna
                    , String.fromInt t.r5sRrna
                    , String.fromInt t.r23sRrna
                    , String.fromInt t.trna
                    , if t.isRepresentative then "yes" else "no"
                    , t.taxonomy
                    ]
                )
    in
        String.concat
            [ String.join "\t" header
            , "\n"
            , List.map (String.join "\t") rows
                |> String.join "\n"
            ]

fetchFastaBytes : MAG -> Cmd Msg
fetchFastaBytes mag =
    Http.request
        { method = "GET"
        , headers = []
        , url = mkFASTALink mag.id
        , body = Http.emptyBody
        , expect = Http.expectBytesResponse (GotFastaBytes mag.id) handleBytesResponse
        , timeout = Nothing
        , tracker = Nothing
        }


handleBytesResponse : Http.Response Bytes -> Result Http.Error Bytes
handleBytesResponse response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)
        Http.Timeout_ ->
            Err Http.Timeout
        Http.NetworkError_ ->
            Err Http.NetworkError
        Http.BadStatus_ metadata _ ->
            Err (Http.BadStatus metadata.statusCode)
        Http.GoodStatus_ _ body ->
            Ok body


buildAndDownloadZip : List MAG -> Dict String Bytes -> Cmd Msg
buildAndDownloadZip magsForDownload fetchedFiles =
    let
        dir = (datasetSlug ++ "-magsview/")

        entryMeta path =
            { path = path
            , lastModified = ( Time.utc, Time.millisToPosix 0 )
            , comment = Nothing
            }

        fastaEntries =
            fetchedFiles
                |> Dict.toList
                |> List.map (\( magId, bytes ) ->
                    Zip.Entry.store (entryMeta (dir ++ magId ++ ".fna.gz")) bytes
                )

        tsvBytes =
            magMetadataTsv magsForDownload
                |> Bytes.Encode.string
                |> Bytes.Encode.encode

        tsvEntry =
            Zip.Entry.store (entryMeta (dir ++ datasetTag ++ "_selected_genomes.metadata.tsv")) tsvBytes

        readmeBytes =
            readmeContent
                |> Bytes.Encode.string
                |> Bytes.Encode.encode

        readmeEntry =
            Zip.Entry.store (entryMeta (dir ++ "README.md")) readmeBytes

        zip =
            Zip.fromEntries (readmeEntry :: tsvEntry :: fastaEntries)
    in
    Download.bytes (datasetTag++"_selected_genomes.zip") "application/zip" (Zip.toBytes zip)


readmeContent : String
readmeContent = mkReadmeFile "Selected Genomes" "selected from the genome browser table" (datasetTag++"_selected_genomes")

magMetadataTsv : List MAG -> String
magMetadataTsv ms =
    let
        header =
            "mag_id\ttaxonomy\tcompleteness\tcontamination\tgenome_size\tnr_contigs\tnr_genes\tis_representative"

        row mag =
            String.join "\t"
                [ mag.id
                , mag.taxonomy
                , String.fromFloat mag.completeness
                , String.fromFloat mag.contamination
                , String.fromInt mag.genomeSize
                , String.fromInt mag.nrContigs
                , String.fromInt mag.nrGenes
                , if mag.isRepresentative then "True" else "False"
                ]
    in
    (header :: List.map row ms)
        |> String.join "\n"


filteredMags : Model -> List MAG
filteredMags model =
    let
        maxNrContigs =
            step2maxContigs model.maxNrContigsStep
    in
        mags
            |> List.sortBy (\t ->
                -- sortBy can receive any comparable value, but it must have a consistent
                -- type, so we use a tuple to sort by either a string or a float
                case model.sortOrder of
                    ById ->
                        (t.id, 0.0)
                    ByTaxonomy ->
                        (t.taxonomy, 0.0)
                    ByCompleteness ->
                        ("", -t.completeness)
                    ByContamination ->
                        ("", t.contamination)
                    ByNrContigs ->
                        ("", toFloat t.nrContigs)
                    ByGenomeSize ->
                        ("", toFloat <| -t.genomeSize)
                        )

            |> (if model.repsOnly
                    then List.filter .isRepresentative
                    else identity)
            |> (if model.hqOnly
                    then List.filter (\t -> magQuality t == High)
                    else identity)
            |> (if String.isEmpty model.taxonomyFilter
                    then identity
                    else List.filter (\t ->
                            String.contains
                                (String.toLower model.taxonomyFilter)
                                (String.toLower t.taxonomy))
                )
            |> (if maxNrContigs > 0
                    then List.filter (\t -> t.nrContigs <= maxNrContigs)
                    else identity)

view :
    Model
    -> View Msg
view model =
    let
        sel = filteredMags model
        maxNrContigs =
            step2maxContigs model.maxNrContigsStep
        theader sortO h =
                Table.th
                    [ Table.cellAttr <| HE.onClick ( SetSortOrder sortO)
                    ] [ Html.a [HtmlAttr.href "#" ] [ Html.text h ] ]
        maybeSimplifyTaxonomy =
            if model.showFullTaxonomy then
                Html.text
            else
                taxonomyLast >> showTaxon
        taxonomyHeader =
            Table.th
                [
                ]
                [ Html.a [HtmlAttr.href "#"
                    , HE.onClick (SetSortOrder ByTaxonomy)
                    ] [ Html.text "Taxonomy (GTDB)" ]
                , Html.a [HtmlAttr.href "#"
                    , HE.onClick ToggleShowFullTaxonomy
                    ] [ Html.text (if model.showFullTaxonomy then " [collapse]" else " [expand]") ]
                ]
    in
        { title = (datasetName ++ ": Genomes table")
        , body =
            [ Html.div []
                [ Html.h1 []
                    [ Html.text "Genomes table" ]
                ]
            , Grid.simpleRow
                (( Grid.col [Col.lg4]
                    [Html.h2 [] [Html.text "Filter genomes"]
                    ,Html.p []
                        [ Html.text ("Selected genomes: " ++ (sel |> List.length |> String.fromInt)
                                    ++ " (of " ++ (mags |> List.length |> String.fromInt) ++ ")")]
                    , Html.p []
                        [ Html.text "Representative genomes only: "
                        , InputCheckbox.view
                            [InputCheckbox.toggle, InputCheckbox.small]
                            { value = model.repsOnly
                            , onInput = SetRepsOnly
                            }
                        ]
                    , Html.p []
                        [ Html.text "High-quality genomes only: "
                        , InputCheckbox.view
                            [InputCheckbox.toggle, InputCheckbox.small]
                            { value = model.hqOnly
                            , onInput = SetHqOnly
                            }
                        ]
                    , Html.p []
                        [ Html.text "Taxonomy filter: "
                        , Html.span []
                            (if model.taxonomyUpActive
                                then
                                    [ Html.a [HE.onClick (UpdateTaxonomyFilter <| upOneLevel model.taxonomyFilter), HtmlAttr.href "#"]
                                        [ Html.text "[go up one level]" ]
                                    ]
                                else
                                    [ ]
                            )
                        , Input.text
                            [ Input.placeholder "Enter taxonomy"
                            , Input.value model.taxonomyFilter
                            , Input.onInput UpdateTaxonomyFilter
                            ]
                        ]
                    , Html.p []
                        [ Html.text "Max # contigs: "
                        , Html.text (
                            if maxNrContigs < 0
                                then "no limit"
                            else if maxNrContigs == 1
                                then "1 (single contig only)"
                            else (String.fromInt maxNrContigs ++ " contigs"))
                        , InputSlider.view []
                            { min = 0
                            , max = 6
                            , step = 1
                            , value = model.maxNrContigsStep
                            , onInput = UpdateMaxNrContigs
                            }
                        ]
                    ]
                )::(viewCharts model sel))
            , Grid.simpleRow [ Grid.col [ ]
                [Html.div [HtmlAttr.style "margin-bottom" "1em"]
                    [ Button.button
                        [ Button.primary
                        , Button.onClick DownloadTSV
                        ]
                        [ Html.text "Download table as TSV" ]
                    , Html.text " "
                    , if List.length sel <= 200 then
                        Button.button
                            [ Button.outlinePrimary
                            , Button.onClick ShowDownloadModal
                            ]
                            [ Html.text ("Download " ++ String.fromInt (List.length sel) ++ " genomes as zip") ]
                      else
                        Button.button
                            [ Button.outlinePrimary
                            , Button.disabled True
                            , Button.attrs [ HtmlAttr.title "Filter to 100 or fewer genomes to enable zip download" ]
                            ]
                            [ Html.text ("Download genomes as zip (max 200, currently " ++ String.fromInt (List.length sel) ++ ")") ]
                    ]
                , Modal.view []
                    { isOpen = model.showDownloadModal
                    , onClose = Just ClearDownload
                    , content =
                        [ viewDownloadModal model.useWget model.downloadState sel ]
                    }
                , Table.table
                    { options = [ Table.striped, Table.hover, Table.responsive ]
                    , thead =  Table.simpleThead
                        [ theader ById "MAG ID"
                        , theader ByCompleteness "Completeness"
                        , theader ByContamination "Contamination"
                        , theader ByNrContigs "#Contigs"
                        , theader ByGenomeSize "Genome size (Mbp)"
                        , taxonomyHeader
                        ]
                    , tbody =
                        sel
                            |> List.map (\t ->
                                Table.tr []
                                    [ Table.td [] [ Html.a
                                                        [ HtmlAttr.href ("/genome/"++ t.id)
                                                        , HtmlAttr.style "font-weight"
                                                            (if t.isRepresentative then "bold" else "normal")
                                                        ]
                                                        [ Html.text t.id ]
                                                    ]
                                    , Table.td [] [ Html.text (t.completeness |> String.fromFloat) ]
                                    , Table.td [] [ Html.text (t.contamination |> String.fromFloat) ]
                                    , Table.td [] [ Html.text (t.nrContigs |> String.fromInt) ]
                                    , Table.td [] [ Html.text (t.genomeSize |> (\s -> toFloat (s // 1000// 10) /100.0) |> String.fromFloat) ]
                                    , Table.td [] [ maybeSimplifyTaxonomy t.taxonomy ]
                                    ])
                            |> Table.tbody []
                    }
            ]]]
        }

viewDownloadModal : Bool -> DownloadState -> List MAG -> Html.Html Msg
viewDownloadModal useWget downloadState ms =
    let
        downloadButton =
            case downloadState of
                Downloading state ->
                    Button.button
                        [ Button.primary
                        , Button.disabled True
                        ]
                        [ Html.text <| "Downloading... ("
                            ++ String.fromInt (Dict.size state.fetched)
                            ++ "/" ++ String.fromInt state.expected ++ ")" ]
                DownloadError err ->
                    Html.span []
                        [ Button.button
                            [ Button.primary
                            , Button.onClick TriggerBulkDownload
                            ]
                            [ Html.text "Retry download" ]
                        , Html.span [HtmlAttr.style "color" "red", HtmlAttr.style "padding-left" "1em"]
                            [ Html.text err ]
                        ]
                NotStarted ->
                    Button.button
                        [ Button.primary
                        , Button.onClick TriggerBulkDownload
                        ]
                        [ Html.text "Download as zip" ]

        mkScript cmd =
            "#!/usr/bin/env bash\nset -e\n\n"
                ++ String.join "" (List.map (\m -> cmd ++ " " ++ mkFASTALink m.id ++ "\n") ms)

        (scriptFilename, scriptContent) =
            if useWget then
                ("download_genomes_wget.sh", mkScript "wget")
            else
                ("download_genomes_curl.sh", mkScript "curl -O")
    in
    Html.div [HtmlAttr.class "download-modal"]
        [ Html.h3 [] [Html.text "Download"]
        , Html.p []
            [ Html.text <| "You are downloading " ++ (
                if List.length ms == 1
                    then "a single genome."
                    else String.fromInt (List.length ms) ++ " genomes.") ]
        , Html.p [] [ downloadButton ]
        , Html.h4 [] [Html.text "Command line download"]
        , Html.p []
            [ ButtonGroup.buttonGroup
                [ ButtonGroup.small ]
                [ ButtonGroup.button
                    [ if useWget then Button.primary else Button.outlineSecondary
                    , Button.onClick (SetUseWget True)
                    ] [ Html.text "wget" ]
                , ButtonGroup.button
                    [ if useWget then Button.outlineSecondary else Button.primary
                    , Button.onClick (SetUseWget False)
                    ] [ Html.text "curl" ]
                ]
            , Html.text " "
            , Button.button
                [ Button.outlineSecondary
                , Button.onClick (DownloadScript scriptFilename scriptContent)
                ]
                [ Html.text "Download script" ]
            ]
        ]


viewCharts model sel =
    [ Grid.col []
        [ Html.div
            [HtmlAttr.style "width" "100px"
            ,HtmlAttr.style "height" "210px"
            ,HtmlAttr.style "margin-left" "50px"
            ]
            [GenomeStats.chartQualitySummary sel]
        ]
    , Grid.col []
        [ Html.div
            [HtmlAttr.style "width" "210px"
            ,HtmlAttr.style "height" "210px"
            ]
            [GenomeStats.chartQualityScatter OnHover model.hovering sel]
        ]
    , Grid.col []
        [ Html.div
            [HtmlAttr.style "width" "210px"
            ,HtmlAttr.style "height" "210px"
            ]
            [GenomeStats.chartNrContigs sel]
        ]
    ]

