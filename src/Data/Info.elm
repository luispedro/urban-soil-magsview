module Data.Info exposing (
    datasetName, datasetTag, datasetSlug,
    mkFASTALink, mkENOGLink, mkEMapperSummaryLink, mkBarrnapLink,
    mkReadmeFile)


datasetName : String
datasetName = "Urban Soil MAGs"

datasetTag : String
datasetTag = "CS1"

datasetSlug : String
datasetSlug = "urban-soil"

baseURL : String
baseURL = "https://urban-soil-mags-data.big-data-biology.org/"

mkFASTALink : String -> String
mkFASTALink mid =
    baseURL ++ "UrbanSoilMAGs/" ++ mid ++ ".fna.gz"

mkENOGLink : String -> String
mkENOGLink mid =
    baseURL ++ "UrbanSoilMAGAnnotations/EMapper/" ++ mid ++ ".emapper.annotations.xz"

mkEMapperSummaryLink : String -> String
mkEMapperSummaryLink mid =
    baseURL ++ "UrbanSoilMAGAnnotations/EMapperSummary/" ++ mid ++ ".emapper_summary.tsv"

mkBarrnapLink : String -> String
mkBarrnapLink mid =
    baseURL ++ "UrbanSoilMAGAnnotations/Barrnap/" ++ mid ++ "_ribosomal.fna.gz"

mkReadmeFile : String -> String -> String -> String
mkReadmeFile subtitle mid table =
    "# Urban Soil MAGs: " ++ subtitle ++ "\n"
        ++ "\n"
        ++ "This archive contains metagenome-assembled genomes (MAGs) from the\n"
        ++ "Urban Soil MAG catalogue, " ++ mid ++ ".\n"
        ++ "\n"
        ++ "## Contents\n"
        ++ "\n"
        ++ "- `*.fna.gz` - Genome FASTA files (gzip-compressed)\n"
        ++ "- `" ++ table ++ ".metadata.tsv` - MAG metadata (TSV format)\n"
        ++ "- `README.md` - This file\n"
        ++ "\n"
        ++ "## Source\n"
        ++ "\n"
        ++ "Data downloaded from the Urban Soil MAG Viewer:\n"
        ++ "https://urban-soil-mags.big-data-biology.org/\n"
        ++ "\n"
        ++ "## Citation\n"
        ++ "\n"
        ++ "Long-read metagenomic sequencing reveals novel lineages and "
        ++ "functional diversity in urban soil microbiome Yiqian Duan, Anna Cuscó, "
        ++ "Yaozhong Zhang, Juan S. Inda-Díaz, Chengkai Zhu, Alexandre Areias "
        ++ "Castro, Xinrun Yang, Jiabao Yu, Gaofei Jiang, Xing-Ming Zhao, Luis "
        ++ "Pedro Coelho\n"
        ++ "bioRxiv 2026.03.20.713087; doi: https://doi.org/10.64898/2026.03.20.713087"


