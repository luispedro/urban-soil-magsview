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
import Data.Info exposing (datasetName, mkFASTALink, mkReadmeFile, datasetTag, datasetSlug)


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

type alias TaxonEntry =
    { name : String
    , displayName : String
    , level : String
    , ancestors : List String
    , count : Int
    }

type alias Model =
    { tree : TreeNode
    , showDownloadModal : Maybe DownloadModal
    , downloadState : DownloadState
    , useWget : Bool
    , searchQuery : String
    , allTaxa : List TaxonEntry
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
    | DownloadScript String String
    | SetUseWget Bool
    | SetSearchQuery String
    | JumpToTaxon (List String)

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
            , useWget = True
            , searchQuery = ""
            , allTaxa = buildAllTaxa mags
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

        DownloadScript filename content ->
            ( model
            , Effect.sendCmd <| Download.string filename "text/x-shellscript" content
            )
        SetUseWget v ->
            ({ model | useWget = v }, Effect.none)

        SetSearchQuery q ->
            ({ model | searchQuery = q }, Effect.none)

        JumpToTaxon path ->
            ( { model
                | tree = expandPath path model.tree
                , searchQuery = ""
              }
            , Effect.none
            )


bestTaxonName : List MAG -> String
bestTaxonName ms =
    case ms of
        [] ->
            "genomes"
        mag :: _ ->
            mag.taxonomy
                |> String.split ";"
                |> List.reverse
                |> List.filter (\level ->
                    case String.split "__" level of
                        [ _, name ] -> not (String.isEmpty name)
                        _ -> False
                )
                |> List.head
                |> Maybe.withDefault "genomes"


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
        dir = (datasetSlug ++ "-magsview/")

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
    Download.bytes (datasetTag ++ "_" ++ safeTaxonName ++ ".zip") "application/zip" (Zip.toBytes zip)


readmeContent : String -> String
readmeContent taxonName =
    mkReadmeFile
        taxonName
        ("filtered by taxonomy (" ++ taxonName ++ ")")
        (String.replace " " "_" taxonName)


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


buildAllTaxa : List MAG -> List TaxonEntry
buildAllTaxa allMags =
    let
        prefixesOf : List String -> List (List String)
        prefixesOf xs =
            List.range 1 (List.length xs)
                |> List.map (\n -> List.take n xs)

        bumpCount : Maybe Int -> Maybe Int
        bumpCount mv =
            Just (1 + Maybe.withDefault 0 mv)

        addPath : List String -> Dict String ( List String, Int ) -> Dict String ( List String, Int )
        addPath path d =
            Dict.update (String.join ";" path)
                (\v ->
                    case v of
                        Just ( p, c ) -> Just ( p, c + 1 )
                        Nothing -> Just ( path, 1 )
                )
                d

        countByPath : Dict String ( List String, Int )
        countByPath =
            allMags
                |> List.concatMap (\m -> prefixesOf (String.split ";" m.taxonomy))
                |> List.foldl addPath Dict.empty
    in
    countByPath
        |> Dict.values
        |> List.filterMap (\( path, count ) ->
            case List.reverse path of
                name :: revAncestors ->
                    let
                        ( level, displayName ) =
                            splitTaxon name
                    in
                    if String.isEmpty displayName then
                        Nothing
                    else
                        Just
                            { name = name
                            , displayName = displayName
                            , level = level
                            , ancestors = List.reverse revAncestors
                            , count = count
                            }
                [] ->
                    Nothing
        )


searchTaxa : String -> List TaxonEntry -> List TaxonEntry
searchTaxa query allTaxa =
    let
        q = String.toLower (String.trim query)
    in
    if String.length q < 2 then
        []
    else
        let
            matchScore entry =
                let
                    name = String.toLower entry.displayName
                in
                if name == q then
                    0
                else if String.startsWith q name then
                    1
                else
                    2
        in
        allTaxa
            |> List.filter (\e -> String.contains q (String.toLower e.displayName))
            |> List.sortBy (\e -> ( matchScore e, String.length e.displayName, e.displayName ))
            |> List.take 30


expandPath : List String -> TreeNode -> TreeNode
expandPath path tree =
    List.foldl (\name t -> expandNode 0 name t) tree path


view :
    Model
    -> View Msg
view model =
    { title = (datasetName ++ ": Taxonomy explorer")
    , body =
        [ Html.div []
            [ Html.h1 []
                [ Html.text "Taxonomy explorer" ]
            ]
        , searchView model
        , showTree [] model.useWget model.showDownloadModal model.downloadState model.tree
        ]
    }


searchView : Model -> Html.Html Msg
searchView model =
    let
        results = searchTaxa model.searchQuery model.allTaxa

        trimmed = String.trim model.searchQuery
    in
    Html.div [ HtmlAttr.class "taxonomy-search" ]
        [ Html.label
            [ HtmlAttr.for "taxonomy-search-input"
            , HtmlAttr.class "taxonomy-search-label"
            ]
            [ Html.text "Jump to taxon: " ]
        , Html.input
            [ HtmlAttr.id "taxonomy-search-input"
            , HtmlAttr.type_ "text"
            , HtmlAttr.value model.searchQuery
            , HtmlAttr.placeholder "e.g. Bacteroides"
            , HtmlAttr.autocomplete False
            , HE.onInput SetSearchQuery
            ]
            []
        , if List.isEmpty results then
            if String.length trimmed >= 2 then
                Html.p [ HtmlAttr.class "taxonomy-search-empty" ]
                    [ Html.text "No matches" ]
            else
                Html.text ""
          else
            Html.ul [ HtmlAttr.class "taxonomy-search-results" ]
                (List.map searchResultRow results)
        ]


searchResultRow : TaxonEntry -> Html.Html Msg
searchResultRow entry =
    let
        targetPath = entry.ancestors ++ [ entry.name ]

        breadcrumbs =
            entry.ancestors
                |> List.filter (\a -> not (String.startsWith "r__" a))
                |> List.map (\a ->
                    let
                        ( _, n ) = splitTaxon a
                    in
                    if String.isEmpty n then "?" else n
                )
                |> String.join " > "
    in
    Html.li
        [ HE.onClick (JumpToTaxon targetPath)
        , HtmlAttr.class "taxonomy-search-result"
        ]
        [ Html.span [ HtmlAttr.class "taxonomy-search-name" ]
            [ Html.text entry.displayName ]
        , Html.span [ HtmlAttr.class "taxonomy-class" ]
            [ Html.text (" (" ++ entry.level ++ ", " ++ String.fromInt entry.count ++ " genomes)") ]
        , if String.isEmpty breadcrumbs then
            Html.text ""
          else
            Html.div [ HtmlAttr.class "taxonomy-search-breadcrumbs" ]
                [ Html.text breadcrumbs ]
        ]

showTree : List String -> Bool -> Maybe DownloadModal -> DownloadState -> TreeNode -> Html.Html Msg
showTree path useWget showDownloadModal downloadState treeNode =
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
            let
                isCollapsed = case treeNode of
                    CollapsedNode _ _ -> True
                    _ -> False
                isLeaf = case treeNode of
                    LeafNode _ _ -> True
                    _ -> False
                disclosure =
                    if isLeaf then
                        ""
                    else if isCollapsed then
                        "▶"
                    else
                        "▼"
                rowAttrs =
                    if isLeaf then
                        []
                    else
                        [ HE.onClick ((if isCollapsed then ExpandNode else CollapseNode) name)
                        , HtmlAttr.class "taxonomy-toggle"
                        ]
            in Html.p rowAttrs
                    [ Html.span [HtmlAttr.class "taxonomy-disclosure"]
                        [Html.text disclosure]
                    , Html.span [HtmlAttr.class "taxonomy-header"]
                        [ if String.isEmpty sname
                            then Html.em [] [Html.text "unassigned"]
                            else Html.text sname]
                    , Html.span [HtmlAttr.class "taxonomy-class"]
                        [Html.text (" ("++tlevel++")")]
                    ]
    in Html.div [ HtmlAttr.class "tree-node"
                , HtmlAttr.class ("taxonomy-node-" ++ tlevel)]
        (card :: (case treeNode of
            CollapsedNode _ children ->
                (if name == "r__Root" && List.length children > 1 then
                    []
                else
                    [ Html.p []
                        [ Html.text "Total number of genomes: "
                        , Html.strong []
                            [ Html.text <| String.fromInt (List.length children) ]
                        , Html.a [ HtmlAttr.href ("/genomes?taxonomy=" ++ pathStr ++ "&taxnav=1")]
                            [ Html.text " [table]" ]
                        ]
                    ]
                )
            ExpandedNode _ children ->
                (List.map (showTree (name::path) useWget showDownloadModal downloadState) children)
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
                    [ Html.text "Bolded elements are the species-representative MAGs."
                    , if String.isEmpty sname && List.length children > 1
                        then Html.em [] [Html.text " Note that is a leaf node containing the unassigned genomes for this lineage; they may not all originate from the same species."]
                        else Html.text ""
                    ]
                , Html.p [HtmlAttr.style "text-align" "right"]
                    [ Html.a [ HtmlAttr.href ("/genomes?taxonomy=" ++ pathStr ++ "&taxnav=1")
                            , HtmlAttr.style "padding-right" "18px"
                            ]
                        [ Html.text "[Genomes in table]" ]
                    , ButtonGroup.buttonGroup
                        [ ButtonGroup.small ]
                        [ ButtonGroup.button [ Button.outlinePrimary, Button.small
                            , Button.onClick (DownloadMAGs (bestTaxonName children) <| List.filter (.isRepresentative) children) ]
                            [ Html.text "Download representatives" ]
                        , ButtonGroup.button
                            [ Button.outlinePrimary , Button.small
                            , Button.onClick (DownloadMAGs (bestTaxonName children) children) ]
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
                            |> makeModal useWget downloadState]
                    }
                ]
        ))

makeModal : Bool -> DownloadState -> List MAG -> Html.Html Msg
makeModal useWget downloadState ms =
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
