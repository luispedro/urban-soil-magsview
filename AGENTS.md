# Repository Guidelines

## Project Structure & Module Organization
Core application code lives in `src/`. Route-backed pages are under `src/Pages/` (for example `src/Pages/Genomes.elm` and `src/Pages/Genome/Genome_.elm`), shared layout code is in `src/Layouts/`, and reusable helpers such as `GenomeStats.elm` and `LoadData.elm` stay at the top of `src/`. Static assets are in `static/`, including CSS and per-genome JSON files in `static/genome-data/`. MAG metadata is baked into `src/Data/Blobs/MAGs.elm`. Treat `.elm-land/` and `dist/` as generated output; prefer editing source files instead.

## Elm–JavaScript Interop
Elm ports are declared in `src/GeneSequence.elm` and wired up in `src/interop.js`. The JS side fetches genome FASTA files on demand, caches them, and handles gene sequence extraction (reverse-complement, translation via NCBI table 11). When adding a new port, add the declaration to `GeneSequence.elm`, subscribe in the `onReady` callback in `interop.js`, and import it in the page module that uses it. Download-style ports (e.g., `downloadGeneFasta`) generate content client-side and trigger a browser download via a temporary Blob URL.

## Build, Test, and Development Commands
Use `npx elm-land server` for local development; it starts the Elm Land dev server. Run `nix build .` to produce the production build used for deployment. Use `./build-deploy.sh` for the full Netlify deployment flow after verifying credentials and site access. When touching formatting-sensitive Elm files, run `node_modules/.bin/elm-format src .elm-land/src --yes`.

## Data Flow & Remote Resources
Gene annotations come from eggNOG-mapper summary TSVs fetched at page load (URL built by `Downloads.mkEMapperSummaryLink`). Per-genome JSON in `static/genome-data/` provides 16S rRNA matches and ARG data. Genome FASTA files (`.fna.gz`) are fetched and decompressed in JS on demand—the first gene click or FASTA download triggers the fetch, and the result is cached for the session. Download links for raw files point to `https://sh-dog-mags-data.big-data-biology.org/`.

## Coding Style & Naming Conventions
Follow existing Elm style: 4-space indentation, `PascalCase` module names, `camelCase` values and functions, and descriptive type aliases for decoded data. Keep route module naming consistent with Elm Land conventions such as `Home_.elm` and `Static_.elm`. Prefer small pure helpers over inline logic in `view` functions. CSS selectors in `static/*.css` should remain lowercase and hyphenated.

## Testing Guidelines
There is no committed automated test suite yet. For every change, at minimum run `nix build .` and manually verify the main flows: `/`, `/genomes`, `/taxonomy`, and at least one `/genome/<MAG_ID>` detail page. If you update MAG data, confirm that `src/Data/Blobs/MAGs.elm` and `static/genome-data/*.json` stay in sync. Future tests should mirror the source area they cover and focus on data decoding and route behavior first.

## Commit & Pull Request Guidelines
Recent history uses short prefixes such as `BUG`, `ENH`, and `MIN` followed by an imperative summary, for example `BUG Handle missing taxonomy better`. Keep commits focused and scoped to one concern. PRs should include a brief description, affected routes or data files, manual verification notes, and screenshots for visible UI changes. Call out any dataset or deployment impact explicitly.
