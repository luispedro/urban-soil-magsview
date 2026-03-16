module Pages.Taxonomy exposing (page, Model, Msg)

import Html
import Html.Attributes as HtmlAttr
import Html.Events as HE
import Set

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

import Json.Encode as E

import DataModel exposing (MAG)
import Data exposing (mags)
import Layouts
import GenomeStats exposing (splitTaxon)
import Downloads exposing (mkFASTALink)
import GeneSequence exposing (downloadMultipleUrls)


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


type alias Model =
    { tree : TreeNode
    , showDownloadModal : Maybe (List MAG)
    , downloadStarted : Bool
    }


type Msg =
    ExpandNode String
    | CollapseNode String
    | DownloadMAGs (List MAG)
    | ClearDownload
    | TriggerBulkDownload

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
            , downloadStarted = False
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
                Just ms ->
                    ( { model | downloadStarted = True }
                    , ms
                        |> List.map (\m -> mkFASTALink m.id)
                        |> E.list E.string
                        |> downloadMultipleUrls
                        |> Effect.sendCmd
                    )
                Nothing ->
                    ( model, Effect.none )
        _ ->
            let
                nmodel = case msg of
                    ExpandNode target -> { model | tree = expandNode 0 target model.tree }
                    CollapseNode target -> { model | tree = collapseNode target model.tree }
                    DownloadMAGs ms -> { model | showDownloadModal = Just (List.sortBy .id ms), downloadStarted = False }
                    ClearDownload -> { model | showDownloadModal = Nothing, downloadStarted = False }
                    TriggerBulkDownload -> model
            in
                ( nmodel
                , Effect.none
                )

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
            , showTree [] model.showDownloadModal model.downloadStarted model.tree
            ]
        }

showTree : List String -> Maybe (List MAG) -> Bool -> TreeNode -> Html.Html Msg
showTree path showDownloadModal downloadStarted treeNode =
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
                (List.map (showTree (name::path) showDownloadModal downloadStarted) children)
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
                            , Button.onClick (DownloadMAGs <| List.filter (.isRepresentative) children) ]
                            [ Html.text "Download representatives" ]
                        , ButtonGroup.button
                            [ Button.outlinePrimary , Button.small
                            , Button.onClick (DownloadMAGs children) ]
                            [ Html.text "Download all" ]
                        ]
                    ]
                , Modal.view []
                    { isOpen = showDownloadModal /= Nothing
                    , onClose = Just ClearDownload
                    , content =
                        [showDownloadModal
                            |> Maybe.withDefault []
                            |> makeModal downloadStarted]
                    }
                ]
        ))

makeModal : Bool -> List MAG -> Html.Html Msg
makeModal downloadStarted ms =
    Html.div [HtmlAttr.class "download-modal"]
        [Html.h3 [] [Html.text "Download"]
        , Html.p []
            [ Html.text <| "You are downloading " ++ (
                if List.length ms == 1
                    then "a single genome."
                    else String.fromInt (List.length ms) ++ " genomes.") ]
        , if List.length ms < 20
            then Html.p []
                [ if downloadStarted
                    then Button.button
                        [ Button.primary
                        , Button.disabled True
                        ]
                        [ Html.text "Downloads started!" ]
                    else Button.button
                        [ Button.primary
                        , Button.onClick TriggerBulkDownload
                        ]
                        [ Html.text "Download all files" ]
                ]
            else Html.text ""
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
