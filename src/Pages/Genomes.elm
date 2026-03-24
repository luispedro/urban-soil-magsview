module Pages.Genomes exposing (page, Model, Msg)

import Dict exposing (Dict)
import Html
import Html.Attributes as HtmlAttr
import Html.Events as HE

import File.Download as Download

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
import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Table as Table

import DataModel exposing (MAG)
import Data exposing (mags)
import GenomeStats exposing (Quality(..), magQuality, taxonomyLast, splitTaxon, showTaxon)
import Shared



type SortOrder =
    ById
    | ByTaxonomy
    | ByCompleteness
    | ByContamination
    | ByNrContigs
    | ByGenomeSize

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
                , Effect.sendCmd <| Download.string "urban_soil_selected_genomes.tsv" "text/tab-separated-values" tsv
                )
        OnHover hovering ->
            ({ model | hovering = hovering }, Effect.none)


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
        { title = "Urban soil MAGs: Genomes table"
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
                [Button.button
                    [ Button.primary
                    , Button.onClick DownloadTSV
                    ]
                    [ Html.text "Download table as TSV" ]
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

