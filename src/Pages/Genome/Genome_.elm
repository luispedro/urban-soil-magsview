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
import Downloads exposing (mkFASTALink, mkENOGLink)

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

type LoadedDataModel =
    Loaded MAGData
    | LoadError String
    | Waiting

type alias Model =
    { magdata : LoadedDataModel
    , expanded : Bool
    , expanded16S : List Int
    , showARGSequences : Bool
    }

type Msg =
    ResultsData (Result Http.Error APIResult)
    | Toggle16SExpanded
    | Expand16S Int
    | ToggleShowARGSequences
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
    , expanded = False
    , expanded16S = []
    , showARGSequences = False
    }

cmd0 : String -> Effect Msg
cmd0 magid =
    Effect.sendCmd
        (Http.get
            { url = "/genome-data/"++ magid ++ ".json"
            , expect = Http.expectJson ResultsData decodeMAGData
            }
        )

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

        _ ->
            ( model
            , Effect.none
            )


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
