module Pages.Home_ exposing (page, Model, Msg)

import View exposing (View)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Effect exposing (Effect)

import SiteMarkdown exposing (mdToHtml)
import Layouts

type alias Model = {}
type Msg = NoOp

page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = \_ -> ({}, Effect.none)
        , update = \_ _ -> ({}, Effect.none)
        , subscriptions = \_ -> Sub.none
        , view = \_ ->
            { title = "Urban soil MAGs"
            , body = [mdToHtml content]
            }
    } |> Page.withLayout (\_ -> Layouts.Main {})

content : String
content = """
## Urban soil microbiome

> _Long-read metagenomic sequencing reveals novel lineages and functional
> diversity in urban soil microbiome_ by Yiqian Duan, Anna Cuscó, Yaozhong Zhang,
> Juan S. Inda-Díaz, Chengkai Zhu, Alexandre Areias Castro, Xinrun Yang, Jiabao Yu,
> Gaofei Jiang, Xing-Ming Zhao, Luis Pedro Coelho (bioRxiv 2026.03.20.713087;
> doi: [10.64898/2026.03.20.713087](https://doi.org/10.64898/2026.03.20.713087))

City parks and other urban green spaces can bring significant benefits to the 
physical and mental health of city residents. Most studies of the urban soil 
microbiome so far have used short-read sequencing, which breaks up genomes and 
misses important pieces like ribosomal genes and mobile elements.

![Urban soil](/images/Fig1a.svg)

For this project, we used deep long-read sequencing (ONT), polished with
short reads (Illumina) on 58 soil samples from university campuses, parks 
located in two major cities from China. This gave us **7,949 high-quality genomes** 
from **4,171 different species**. In total, 1,060 MAGs are close to finished 
quality, _often better than the available reference genomes_.

In addition to the genomes, we revealed extensive secondary metabolic capacity and 
uncovered over 2 million small protein families.
"""
