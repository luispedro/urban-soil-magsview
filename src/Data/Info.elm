module Data.Info exposing (
    datasetName, datasetTag, datasetSlug,
    mkFASTALink, mkENOGLink, mkEMapperSummaryLink, mkBarrnapLink,
    mkReadmeFile)


datasetName : String
datasetName = "Shanghai Dog Gut MAGs"

datasetTag : String
datasetTag = "SHD1"

datasetSlug : String
datasetSlug = "sh-dogs"

baseURL : String
baseURL = "https://sh-dog-mags-data.big-data-biology.org/"

mkFASTALink : String -> String
mkFASTALink mid =
    baseURL ++ "ShanghaiDogsMAGs/" ++ mid ++ ".fna.gz"

mkENOGLink : String -> String
mkENOGLink mid =
    baseURL ++ "ShanghaiDogsMAGAnnotations/EMapper/" ++ mid ++ ".emapper.annotations.xz"

mkEMapperSummaryLink : String -> String
mkEMapperSummaryLink mid =
    baseURL ++ "ShanghaiDogsMAGAnnotations/EMapperSummary/" ++ mid ++ ".emapper_summary.tsv"

mkBarrnapLink : String -> String
mkBarrnapLink mid =
    baseURL ++ "ShanghaiDogsMAGAnnotations/Barrnap/" ++ mid ++ "_ribosomal.fna.gz"

mkReadmeFile : String -> String -> String -> String
mkReadmeFile subtitle mid table =
    "# Shanghai Dog Gut MAGs: " ++ subtitle
        ++ "\n"
        ++ "This archive contains metagenome-assembled genomes (MAGs) from the\n"
        ++ "Shanghai Dog Gut MAG catalogue, " ++ mid ++ ".\n"
        ++ "\n"
        ++ "## Contents\n"
        ++ "\n"
        ++ "- `*.fna.gz` - Genome FASTA files (gzip-compressed)\n"
        ++ "- `"++ table ++ ".metadata.tsv` - MAG metadata (TSV format)\n"
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
