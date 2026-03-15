# eggNOG-mapper Summary TSV Format

Each MAG has an associated gene summary file in TSV format, produced by
summarising [eggNOG-mapper](http://eggnog-mapper.embl.de/) output. These files
are hosted at:

    https://sh-dog-mags-data.big-data-biology.org/ShanghaiDogsMAGAnnotations/EMapperSummary/<MAG_ID>.emapper_summary.tsv

The application fetches them at runtime to populate the genome map and gene
detail panel.

## Columns

The file is tab-separated with a single header row. Columns are:

| # | Column            | Type   | Description                                                                 |
|---|-------------------|--------|-----------------------------------------------------------------------------|
| 1 | `seqid`           | string | Gene identifier (e.g., `SHD1_0006_1_1`). Unique within the MAG.            |
| 2 | `contig`          | string | Contig the gene belongs to (e.g., `SHD1_0006_1`).                          |
| 3 | `start`           | int    | Start position of the gene on the contig (1-based, in bp).                 |
| 4 | `end`             | int    | End position of the gene on the contig (1-based, in bp).                   |
| 5 | `strand`          | string | Strand: `+` (forward) or `-` (reverse).                                   |
| 6 | `COG_category`    | string | COG functional category letter(s) (e.g., `L`, `KT`). `-` if unassigned.   |
| 7 | `Preferred_name`  | string | Human-readable gene name from eggNOG-mapper. `-` if unavailable.           |
| 8 | `KEGG_ko`         | string | Comma-separated KEGG Orthology identifiers (e.g., `ko:K00001,ko:K00002`). `-` if none. |
| 9 | `KEGG_Module`     | string | Comma-separated KEGG Module identifiers (e.g., `M00001`). `-` if none.    |

## Example

```tsv
seqid	contig	start	end	strand	COG_category	Preferred_name	KEGG_ko	KEGG_Module
SHD1_0006_1_1	SHD1_0006_1	3	560	-	L	-	-	-
SHD1_0006_1_2	SHD1_0006_1	724	1797	-	T	-	-	-
```

## Notes

- A `-` in any field means the value is absent or unassigned.
- When multiple COG categories apply to a single gene, they are concatenated
  without a separator (e.g., `KT`). The application uses the first letter for
  colouring.
- KEGG KO and Module entries are comma-separated when a gene maps to more than
  one.
- The file is used to render the genome map (contig backbone lines with gene
  arrows), the gene detail panel (annotations, local neighborhood), and the
  gene table.
