module Pages.Taxonomy exposing (page, Model, Msg)

import Bytes exposing (Bytes)
import Bytes.Encode
import Dict exposing (Dict)
import File.Download as Download
import Html
import Html.Attributes as HtmlAttr
import Html.Events as HE
import Http
import Set
import Time
import Zip
import Zip.Entry

import Route exposing (Route)
import Page exposing (Page)
import View exposing (View)

import Shared
import Effect exposing (Effect)
import View exposing (View)

import W.InputCheckbox as InputCheckbox
import W.Modal as Modal
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Table as Table

import DataModel exposing (MAG)
import Data exposing (mags)
import Layouts
import GenomeStats exposing (splitTaxon)
import Downloads exposing (mkFASTALink)


type TreeNode =
    CollapsedNode String (List MAG)
    | ExpandedNode String (List TreeNode)
    | LeafNode String (List MAG)

nameOf : TreeNode -> String
nameOf treeNode =
    case treeNode of
        CollapsedNode name _ -> name
        ExpandedNode name _ -> name
        LeafNode name _ -> name


type alias DownloadModal =
    { taxonName : String
    , mags : List MAG
    }

type alias Model =
    { tree : TreeNode
    , showDownloadModal : Maybe DownloadModal
    , downloadState : DownloadState
    }

type DownloadState
    = NotStarted
    | Downloading { expected : Int, fetched : Dict String Bytes }
    | DownloadError String


type Msg =
    ExpandNode String
    | CollapseNode String
    | DownloadMAGs String (List MAG)
    | ClearDownload
    | TriggerBulkDownload
    | GotFastaBytes String (Result Http.Error Bytes)

type alias RouteParams =
    {}

page : Shared.Model -> Route () -> Page Model Msg
page shared r =
    Page.new
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
    |> Page.withLayout (\_ -> Layouts.Main {})


type alias Data =
    { mags : List MAG }

type alias ActionData =
    {}


init :
    ()
    -> (Model, Effect Msg)
init _ =
    let
        model =
            { tree = expandNode 0 "r__Root" <| CollapsedNode "r__Root" mags
            , showDownloadModal = Nothing
            , downloadState = NotStarted
            }
    in
        ( model
        , Effect.none
        )
update :
    Msg
    -> Model
    -> (Model, Effect Msg)
update msg model =
    case msg of
        TriggerBulkDownload ->
            case model.showDownloadModal of
                Just modal ->
                    ( { model | downloadState = Downloading { expected = List.length modal.mags, fetched = Dict.empty } }
                    , modal.mags
                        |> List.map fetchFastaBytes
                        |> Cmd.batch
                        |> Effect.sendCmd
                    )
                Nothing ->
                    ( model, Effect.none )

        GotFastaBytes magId result ->
            case model.downloadState of
                Downloading state ->
                    case result of
                        Ok bytes ->
                            let
                                newFetched = Dict.insert magId bytes state.fetched
                            in
                            if Dict.size newFetched == state.expected then
                                let
                                    modal = model.showDownloadModal
                                        |> Maybe.withDefault { taxonName = "genomes", mags = [] }
                                in
                                ( { model | downloadState = NotStarted }
                                , buildAndDownloadZip modal.taxonName modal.mags newFetched
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

        ExpandNode target ->
            ( { model | tree = expandNode 0 target model.tree }, Effect.none )

        CollapseNode target ->
            ( { model | tree = collapseNode target model.tree }, Effect.none )

        DownloadMAGs taxonName ms ->
            ( { model | showDownloadModal = Just { taxonName = taxonName, mags = List.sortBy .id ms }, downloadState = NotStarted }, Effect.none )

        ClearDownload ->
            ( { model | showDownloadModal = Nothing, downloadState = NotStarted }, Effect.none )


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


buildAndDownloadZip : String -> List MAG -> Dict String Bytes -> Cmd Msg
buildAndDownloadZip taxonName magsForDownload fetchedFiles =
    let
        dir = "sh-dogs-magsview/"

        safeTaxonName =
            taxonName
                |> String.replace " " "_"

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
            Zip.Entry.store (entryMeta (dir ++ safeTaxonName ++ ".metadata.tsv")) tsvBytes

        readmeBytes =
            readmeContent taxonName
                |> Bytes.Encode.string
                |> Bytes.Encode.encode

        readmeEntry =
            Zip.Entry.store (entryMeta (dir ++ "README.md")) readmeBytes

        zip =
            Zip.fromEntries (readmeEntry :: tsvEntry :: fastaEntries)
    in
    Download.bytes (safeTaxonName ++ ".zip") "application/zip" (Zip.toBytes zip)


readmeContent : String -> String
readmeContent taxonName =
    "# Shanghai Dog Gut MAGs: " ++ taxonName ++ "\n"
        ++ "\n"
        ++ "This archive contains metagenome-assembled genomes (MAGs) from the\n"
        ++ "Shanghai Dog Gut MAG catalogue, filtered by taxonomy: " ++ taxonName ++ ".\n"
        ++ "\n"
        ++ "## Contents\n"
        ++ "\n"
        ++ "- `*.fna.gz` - Genome FASTA files (gzip-compressed)\n"
        ++ "- `" ++ String.replace " " "_" taxonName ++ ".metadata.tsv` - MAG metadata (TSV format)\n"
        ++ "- `README.md` - This file\n"
        ++ "\n"
        ++ "## Source\n"
        ++ "\n"
        ++ "Data downloaded from the Shanghai Dog Gut MAG Viewer:\n"
        ++ "https://sh-dog-mags.big-data-biology.org/\n"
        ++ "\n"
        ++ "## Citation\n"
        ++ "\n"
        ++ "Cusco, A., Duan, Y., Gil, F., Chklovski, A., Kruthi, N., Pan, S.,\n"
        ++ "Forslund, S., Lau, S., Lober, U., Zhao, X.-M., and Coelho, L.P.\n"
        ++ "\"Capturing global pet dog gut microbial diversity and hundreds of\n"
        ++ "near-finished bacterial genomes by using long-read metagenomics in a\n"
        ++ "Shanghai cohort\" (bioRxiv PREPRINT 2025)\n"
        ++ "DOI: https://doi.org/10.1101/2025.09.17.676595\n"


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

expand1 : Int -> List MAG -> List TreeNode
expand1 level mags =
    let
        getTaxon : Int -> MAG -> String
        getTaxon ell mag =
            mag.taxonomy
                |> String.split ";"
                |> getIx ell
        getIx : Int -> List String -> String
        getIx i =
            List.drop i
                >> List.head
                >> Maybe.withDefault ""
        taxa : List String
        taxa = mags
                |> List.map (getTaxon level)
                |> Set.fromList
                |> Set.toList
        isSingle : Bool
        isSingle = List.length taxa == 1
    in
        taxa
            |> List.map (\t ->
                    mags
                        |> List.filter (\m -> getTaxon level m == t)
                        |> if String.startsWith "s__" t
                            then LeafNode t
                            else if isSingle
                            then ExpandedNode t << expand1 (level + 1)
                            else CollapsedNode t
                    )

expandNode : Int -> String -> TreeNode -> TreeNode
expandNode level target treeNode =
    case treeNode of
        CollapsedNode name children ->
            if name == target then
                ExpandedNode name (expand1 level children)
            else
                treeNode
        ExpandedNode name children ->
            ExpandedNode name
                (List.map (expandNode (level + 1) target) children)
        LeafNode _ _ ->
            treeNode

collapseNode : String -> TreeNode -> TreeNode
collapseNode target treeNode =
    case treeNode of
        CollapsedNode name children -> treeNode
        ExpandedNode name children ->
            if name == target then
                CollapsedNode name (getAllMAGs children)
            else
                ExpandedNode name (List.map (collapseNode target) children)
        LeafNode _ _ ->
            treeNode

getAllMAGs : List TreeNode -> List MAG
getAllMAGs =
        List.map (\child ->
                case child of
                    CollapsedNode _ mags -> mags
                    ExpandedNode _ mags -> getAllMAGs mags
                    LeafNode _ mags -> mags
            )
        >> List.concat


view :
    Model
    -> View Msg
view model =
    let
        m = model
    in
        { title = "Urban soil MAGs: Taxonomy explorer"
        , body =
            [ Html.div []
                [ Html.h1 []
                    [ Html.text "Taxonomy explorer" ]
                ]
            , showTree [] model.showDownloadModal model.downloadState model.tree
            ]
        }

showTree : List String -> Maybe DownloadModal -> DownloadState -> TreeNode -> Html.Html Msg
showTree path showDownloadModal downloadState treeNode =
    let
        name : String
        name = nameOf treeNode
        (tlevel, sname) = splitTaxon name
        pathStr : String
        pathStr =
            (name::path)
                |> List.reverse
                |> List.filter (\x -> not (String.startsWith "r__" x))
                |> String.join ";"
        card =
            let isC = case treeNode of
                    CollapsedNode _ _ -> True
                    ExpandedNode _ _ -> False
                    LeafNode _ _ -> False
                isL = case treeNode of
                    CollapsedNode _ _ -> False
                    ExpandedNode _ _ -> False
                    LeafNode _ _ -> True
            in Html.p []
                    [ Html.span [HtmlAttr.class "taxonomy-header"]
                        [ if String.isEmpty sname
                            then Html.em [] [Html.text "unnamed"]
                            else Html.text sname]
                    , Html.span [HtmlAttr.class "taxonomy-class"]
                        [Html.text (" ("++tlevel++")")]
                    , if isL
                        then Html.span [] []
                        else Html.span
                            [HE.onClick ((if isC then ExpandNode else CollapseNode) name)
                            , HtmlAttr.style "cursor" "pointer"
                            ]
                            [ Html.text (" ["++ (if isC then "+" else "-")++ "]")]
                    ]
    in Html.div [ HtmlAttr.class "tree-node"
                , HtmlAttr.class ("taxonomy-node-" ++ tlevel)]
        (card :: (case treeNode of
            CollapsedNode _ children ->
                ( Html.p []
                    [ Html.text ("Number of genomes: " ++ String.fromInt (List.length children))
                    ]
                ::
                (if name == "r__Root" && List.length children > 1 then
                    []
                else
                    [ Html.p []
                        [ Html.a [ HtmlAttr.href ("/genomes?taxonomy=" ++ pathStr ++ "&taxnav=1")]
                            [ Html.text "[Genomes in table]" ]
                        ]
                    ]
                ))
            ExpandedNode _ children ->
                (List.map (showTree (name::path) showDownloadModal downloadState) children)
            LeafNode _ children ->
                [ Html.ol []
                    ( children
                        |> List.sortBy .id
                        |> List.map (\mag ->
                            Html.li (if mag.isRepresentative
                                        then [HtmlAttr.class "representative"]
                                        else [])
                                [ Html.a [ HtmlAttr.href ("/genome/"++mag.id)]
                                    [ Html.text <|
                                            (mag.id ++ " (" ++ String.fromFloat mag.completeness ++ "% completeness/" ++ String.fromFloat mag.contamination ++ "% contamination)")
                                    ]
                                ]
                        )
                    )
                , Html.p [HtmlAttr.style "font-size" "small"]
                    [ Html.text "Bolded elements are the species-representative MAGs" ]
                , Html.p [HtmlAttr.style "text-align" "right"]
                    [ Html.a [ HtmlAttr.href ("/genomes?taxonomy=" ++ pathStr ++ "&taxnav=1")
                            , HtmlAttr.style "padding-right" "18px"
                            ]
                        [ Html.text "[Genomes in table]" ]
                    , ButtonGroup.buttonGroup
                        [ ButtonGroup.small ]
                        [ ButtonGroup.button [ Button.outlinePrimary, Button.small
                            , Button.onClick (DownloadMAGs name <| List.filter (.isRepresentative) children) ]
                            [ Html.text "Download representatives" ]
                        , ButtonGroup.button
                            [ Button.outlinePrimary , Button.small
                            , Button.onClick (DownloadMAGs name children) ]
                            [ Html.text "Download all" ]
                        ]
                    ]
                , Modal.view []
                    { isOpen = showDownloadModal /= Nothing
                    , onClose = Just ClearDownload
                    , content =
                        [showDownloadModal
                            |> Maybe.map .mags
                            |> Maybe.withDefault []
                            |> makeModal downloadState]
                    }
                ]
        ))

makeModal : DownloadState -> List MAG -> Html.Html Msg
makeModal downloadState ms =
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
    in
    Html.div [HtmlAttr.class "download-modal"]
        [Html.h3 [] [Html.text "Download"]
        , Html.p []
            [ Html.text <| "You are downloading " ++ (
                if List.length ms == 1
                    then "a single genome."
                    else String.fromInt (List.length ms) ++ " genomes.") ]
        , Html.p [] [ downloadButton ]
        ,Html.h4 [] [Html.text "Download links"]
        ,Html.ol []
            (List.map (\m ->
                Html.li []
                    [Html.a [HtmlAttr.href (mkFASTALink m.id)] [Html.text m.id]]
                ) ms)
        ,Html.h4 [] [Html.text "Command line download"]
        ,Html.pre []
            ((Html.text "# Run this command to download the genomes:\n")::
            List.map (\m ->
                Html.text <| "wget " ++ mkFASTALink m.id ++ "\n"
            ) ms)]
