module Pages.Static_ exposing (page, Model, Msg)

import View exposing (View)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Effect exposing (Effect)

import SiteMarkdown exposing (mdToHtml)
import Layouts

type alias Model = {}
type Msg = NoOp

page : Shared.Model -> Route { static : String } -> Page Model Msg
page shared route =
    Page.new
        { init = \_ -> ({}, Effect.none)
        , update = \_ _ -> ({}, Effect.none)
        , subscriptions = \_ -> Sub.none
        , view = \_ ->
            { title = "Urban soil MAGs: About"
            , body = [mdToHtml (content route.params.static)]
            }
    } |> Page.withLayout (\_ -> Layouts.Main {})


content : String -> String
content key = case key of
    "about" ->
        contentAbout
    "other" ->
        contentOther
    "manuscript" ->
        contentManuscript
    _ ->
        contentOther

contentAbout : String
contentAbout = """
## Urban soil microbiome

This was a project led by _Yiqian Duan_ (Fudan University) in the [Big Data Biology Lab](https://big-data-biology.org) led by _Luis Pedro Coelho_. The project is currently being finalized for publication. The data will also be made available at a suitable repository.

Please contact us if you are interested in the data: [yqduan20@fudan.edu.cn](mailto:yqduan20@fudan.edu.cn) or [luispedro@big-data-biology.org](mailto:luispedro@big-data-biology.org).
"""

contentOther : String
contentOther = """
## Urban soil microbiome



Please contact us if you are interested in the data: [yqduan20@fudan.edu.cn](mailto:yqduan20@fudan.edu.cn) or [luispedro@big-data-biology.org](mailto:luispedro@big-data-biology.org).

### MAG catalogue

The MAG catalogue includes:

1. The MAGs themselves, in FASTA format.
2. A table with the MAGs, their size, completeness, contamination, and other statistics.
3. Ribosomal genes of MAGs. 

### Gene catalogue

The gene catalogue includes:

1. The set of genes after clustering at 100% amino acid identity, in FASTA format.
2. Eggnog-mapper annotations for the genes.
3. RGI annotations for the genes (Antimicrobial Resistance Genes).
4. The table contains the genes and the contigs from which they originate.

### Small protein catalogue

The small protein catalogue includes:

1. The set of small proteins after clustering at 100% amino acid identity, in FASTA format.
2. The table contains the small proteins and the contigs from which they originate.

### Other tables

Other tables, including contextual data tables, will be available as
Supplementary Tables in the manuscript.

"""

contentManuscript : String
contentManuscript = """
## Manuscript

> _Long-read metagenomic sequencing reveals novel lineages and functional
> diversity in urban soil microbiome_ by Yiqian Duan, Anna Cuscó, Yaozhong Zhang,
> Juan S. Inda-Díaz, Chengkai Zhu, Alexandre Areias Castro, Xinrun Yang, Jiabao Yu,
> Gaofei Jiang, Xing-Ming Zhao, Luis Pedro Coelho (bioRxiv 2026.03.20.713087;
> doi: [10.64898/2026.03.20.713087](https://doi.org/10.64898/2026.03.20.713087))

"""
