module GenomeStats exposing (chartNrContigs,
                            chartQualitySummary,
                            chartQualityScatter,
                            Quality(..), magQuality, qualityString,
                            taxonomyLast,
                            printableTaxonomy,
                            splitTaxon,
                            showTaxon)


import Html
import Html.Attributes as HtmlAttr
import Svg as S
import Svg.Attributes as SA
import Svg.Events as SE

import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Chart.Item as CI
import DataModel exposing (MAG)


type Quality
    = High
    | Medium
magQuality : MAG -> Quality
magQuality mag =
    if mag.completeness > 90.0 && mag.contamination < 5.0 then
        High
    else Medium

qualityString : Quality -> String
qualityString quality =
    case quality of
        High ->
            "High"

        Medium ->
            "Medium"


chartNrContigs sel =
    let
        groups = sel
            |> List.map .nrContigs
            |> List.map (\x ->
                    if x < 2
                        then String.fromInt x
                    else if x < 6
                        then "2-5"
                    else if x < 11
                        then "6-10"
                    else if x < 21
                        then "11-20"
                    else if x < 51
                        then "21-50"
                    else if x < 100
                        then "51-99"
                    else "100+")
        data =
            ["1", "2-5", "6-10", "11-20", "21-50", "51-99", "100+"]
                |> List.map (\x -> { c = (groups |> List.filter ((==) x) |> List.length |> toFloat)
                                    , label = x })

    in
        C.chart
        [ CA.width 210
        , CA.height 210
        ]
        [ C.yLabels [ CA.withGrid ]
        , C.binLabels .label [ CA.moveDown 14, CA.fontSize 10 ]
        , C.labelAt .min CA.middle [ CA.moveLeft 42, CA.rotate 90 ]
              [ S.text "Number of MAGs" ]
        , C.labelAt CA.middle .min [ CA.moveDown 30 ]
              [ S.text "Number of contigs in genome" ]
        , C.bars []
            [ C.bar .c []
            ]
            data
        ]


chartQualitySummary sel =
    let
        high = List.filter (\m -> magQuality m == High) sel
        medium = List.filter (\m -> magQuality m == Medium) sel

        high_r = high |> List.filter .isRepresentative |> List.length |> toFloat
        high_n = high |> List.filter (.isRepresentative >> not) |> List.length |> toFloat
        medium_r = medium |> List.filter .isRepresentative |> List.length |> toFloat
        medium_n = medium |> List.filter (.isRepresentative >> not) |> List.length |> toFloat
    in
        C.chart
        [ CA.width 100
        , CA.height 210
        ]
        [ C.yLabels [ CA.withGrid ]
        , C.binLabels .label [ CA.moveDown 20 ]
        , C.labelAt .min CA.middle [ CA.moveLeft 56, CA.rotate 90 ]
              [ S.text "Number of MAGs" ]
        , C.labelAt CA.middle .min [ CA.moveDown 40 ]
              [ S.text "Quality" ]

        , C.bars []
            [ C.stacked
                [ C.bar .cn [CA.color "orange"]
                , C.bar .cr [CA.color "green"]
                ]
            ]
                [ { cr = high_r, cn = high_n, label = "High"}
                , { cr = medium_r, cn = medium_n, label = "Medium"}
                ]
        ]

chartQualityScatter onHover hovering sel =
  C.chart
    [ CA.width 210
    , CA.height 210

    , CE.onMouseMove onHover (CE.getNearest CI.dots)
    , CE.onMouseLeave (onHover [])
    ]
    [ C.xLabels [ CA.withGrid ]
    , C.yLabels [ CA.withGrid ]

    , C.labelAt .min CA.middle [ CA.moveLeft 35, CA.rotate 90 ]
          [ S.text "Contamination" ]
    , C.labelAt CA.middle .min [ CA.moveDown 30 ]
          [ S.text "Completeness" ]
     , C.legendsAt .min .max
              [ CA.column
              , CA.spacing 2
              , CA.background "#cccccc33"
              , CA.border "gray"
              , CA.borderWidth 1
              , CA.moveUp 40
              , CA.moveLeft 80
              , CA.htmlAttrs
                  [ HtmlAttr.style "padding" "2px"
                  , HtmlAttr.style "border-radius" "12px"
                  ]
              ]
              []
    , C.each hovering <| \p item ->
        [ C.tooltip item [] []
            [
                let
                    m = CI.getData item
                in
                    Html.span [
                        HtmlAttr.style "font-weight"
                            (if m.isRepresentative
                                then "bold"
                                else "normal")
                        ] [
                       Html.text <| taxonomyLast m.taxonomy]
            ]
        ]

    -- draw the non-representative genomes first
    -- so that the representative genomes are on top
    , C.series .completeness
        [C.scatter .contamination
            [CA.color "orange", CA.size 1]
              |> C.amongst hovering (\datum -> [ CA.highlight 0.5 ])
              |> C.named "Non-representative"
        ] (sel |> List.filter (.isRepresentative >> not))

    , C.series .completeness
        [C.scatter .contamination [CA.color "green", CA.size 1]
              |> C.amongst hovering (\datum -> [ CA.highlight 0.5 ])
              |> C.named "Representative"

        ] (sel |> List.filter .isRepresentative)
    ]

taxonomyLast t =
    String.split ";" t
        |> List.reverse
        |> List.filter (\s -> String.length s > 3)
        |> List.head
        |> Maybe.withDefault ""

printableTaxonomy =
    taxonomyLast >> String.dropLeft 3

splitTaxon : String -> (String, String)
splitTaxon name =
    let
        tlevel = case String.split "__" name of
            [] -> "root"
            (x::_) -> case x of
                "r" -> "root"
                "d" -> "domain"
                "k" -> "kingdom"
                "p" -> "phylum"
                "c" -> "class"
                "o" -> "order"
                "f" -> "family"
                "g" -> "genus"
                "s" -> "species"
                _ -> "unknown"
        sname = case String.split "__" name of
            [_, x] -> String.replace "_" " " x
            _ -> ""
    in
        (tlevel, sname)

showTaxon : String -> Html.Html msg
showTaxon name =
    let
        (tlevel, sname) = splitTaxon name
    in
        Html.span []
            [Html.text sname
            , Html.span [HtmlAttr.class "taxonomy-class"]
                [Html.text (" ("++tlevel++")")]]

