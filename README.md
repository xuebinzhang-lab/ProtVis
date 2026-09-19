# ProtVis

[![](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![](https://img.shields.io/badge/GitHub-ProtVis-blue.svg)](https://github.com/xuebinzhang-lab/ProtVis)
[![](https://img.shields.io/badge/R-Shiny-orange.svg)](https://github.com/xuebinzhang-lab/ProtVis)
[![](https://img.shields.io/badge/platform-all-brightgreen.svg)](https://github.com/xuebinzhang-lab/ProtVis)
[![](https://img.shields.io/badge/license-MIT-green.svg)](https://github.com/xuebinzhang-lab/ProtVis/blob/dev/LICENSE)

<img src="https://github.com/xuebinzhang-lab/ProtVis/blob/dev/app/www/ProtVis_ico.png?raw=true" alt="ProtVis Logo" align="right" width="180"/>

**ProtVis: interactive visualization and downstream interpretation for proteomics and metaproteomics data**

ProtVis is an R/Shiny platform for exploring, processing, visualizing, and interpreting proteomics datasets. It provides graphical workflows for data import, preprocessing, differential protein analysis, enrichment analysis, pathway visualization, multi-omics exploration, protein-level utilities, and metaproteomics taxonomy-function interpretation.

The application is designed for researchers who need publication-ready visual summaries without writing large amounts of custom R code.

<br clear="right"/>

------------------------------------------------------------------------

## Key features

-   **Multi-source proteomics import** for MaxQuant, Proteome Discoverer, DIA-NN, Spectronaut, FragPipe, Skyline, OpenMS, and user-defined matrices.
-   **ProtVis_dataset schema v3** with a canonical protein assay, PSM/peptide assay registry, aligned sample/protein metadata, annotations, analysis results, checkpoints, and backward-compatible migration. Optional `as_QFeatures()` / `from_QFeatures()` helpers connect ProtVis to the Bioconductor QFeatures ecosystem.
-   **Structured provenance** records R/ProtVis/package versions, Sage version and parameters, MD5 file fingerprints, timestamps, node status, errors, and parent/object lineage. Portable exports include `provenance.json`, `provenance.rds`, and `workflow_status.csv`.
-   **Recoverable dependency-aware workflow nodes** for QC filtering, transformation, imputation, normalization, dimensionality reduction, differential analysis, enrichment, and network analysis. Re-running an upstream node explicitly invalidates downstream results; failed nodes can be retried or resumed from checkpoints.
-   **Project QC Dashboard** with protein/sample counts, data completeness, median protein CV, sample-level missingness/identification QC, workflow state, input-file provenance, and Sage search QC.
-   **PSM Explorer** for Protein → Peptide → PSM → MS/MS inspection with mzIdentML/MGF loading, searchable PSM selection, PTM-aware theoretical fragments, matched b/y ions, and exportable annotated spectra.
-   **Headless/CLI mode** via `ProtVis::run_protvis_cli()` or the installed `exec/protvis` script, using the same import, Sage, processing, checkpoint, provenance, and export backend as Shiny.
-   **MaxQuant output preparation** as the first item in **Pre-processing** for MaxQuant-specific filtering and matrix handoff; other sources use their own parser-backed import path.
-   **Protein-level downstream analysis** including DEP analysis, enrichment analysis, GSEA, KEGG/pathway visualization, PPI, WGCNA, co-enrichment, Venn analysis, and expression profiling.
-   **Metaproteomics module** with built-in demo data for abundance, taxonomy, and functional annotations.
-   **Taxonomy-function visualization** including composition plots, Sankey diagrams, and heatmaps for metaproteomics interpretation.
-   **Interactive Shiny interface** for users who prefer GUI-driven analysis and figure generation.
-   **Optional RAW/mzML registration and Sage search preparation** with built-in PXD065315 sample metadata, directory/file consistency checks, and protein FASTA upload.
-   **Uploadable Sage sample metadata**: a sample table containing `sample_id`, `mzml_file`, and grouping fields automatically enters the Sage staging workflow and can be downloaded as [`PXD065315_sample_info_template.csv`](https://github.com/xuebinzhang-lab/ProtVis/blob/dev/inst/extdata/PXD065315_sample_info_template.csv).

------------------------------------------------------------------------

## Supported data sources

ProtVis supports the following data-source options from **Project init → Select data source**:

| Data source | Expected input | Notes |
|---|---|---|
| Raw | Generic expression matrix | Use when data are already organized as `ID + sample intensity columns`. |
| MaxQuant | MaxQuant protein group/expression output | Includes MaxQuant-specific unreliable peptide filtering. |
| DIA-NN | DIA-NN report files | Supports long Protein.Group/Run/quantity reports and wide matrices. |
| Spectronaut | Spectronaut report files | Supports PG.ProteinGroups and quantity/intensity columns. |
| FragPipe | FragPipe protein report files | Parses protein identifiers and intensity/LFQ columns. |
| Proteome Discoverer | Protein or peptide group export (`.xlsx`, `.xls`, `.csv`) | Parses accession/protein ID columns and `Abundance`/`Area` style quantitative columns. |
| Skyline | Skyline report / MSstats-style export (`.csv`, `.tsv`, `.txt`, `.xlsx`, `.xls`) | Supports long reports with `ProteinName`, `FileName`/replicate, and `Area`, or wide protein abundance tables. |
| Mascot | Mascot CSV/export table (`.csv`, `.tsv`, `.txt`, `.xlsx`, `.xls`) | Parses protein accession columns and numeric quantitation columns such as emPAI or intensity. |
| OpenMS | OpenMS consensus / ProteinQuantifier table (`.csv`, `.tsv`, `.txt`, `.xlsx`, `.xls`) | Parses protein accession columns and `intensity`/`abundance`/`area` style columns. |

Parser-backed imports are converted into a common ProtVis expression matrix and sample metadata schema. When a working directory is available, ProtVis writes preprocessing-compatible handoff files so the imported data can continue into the **Pre-processing** menu.

The ProtVis_dataset tab also loads bundled examples for every supported source
without requiring an upload. `protvis_builtin_datasets()` lists the files,
formats, descriptions, and official documentation links. The fixtures are
deliberately compact (5,000 protein groups for the source adapters) and include:

- MaxQuant `Maxquant_Export.xlsx`
- Proteome Discoverer `ProteomeDiscoverer_proteins.txt`
- DIA-NN `DIA-NN_report.tsv`
- Spectronaut `Spectronaut_report.tsv`
- FragPipe `FragPipe_combined_protein.tsv`
- Skyline `Skyline_report.csv`
- OpenMS `OpenMS_protein_quantification.tsv`
- HUPO-PSI mzTab `OpenMS_proteins.mzTab`

Each fixture is parsed into `ProtVis_dataset` and can continue through the
same QC, transformation, imputation, normalization, dimensionality reduction,
differential analysis, enrichment, network, checkpoint, and export functions.
The full MaxQuant workbook remains available for a larger reproducible example.

### RAW/mzML and Sage database-search preparation

Project init contains an optional, collapsed **RAW/mzML input** panel. Select
the directory containing converted mzML files, load the built-in PXD065315
sample information, and click **Check mzML files**. The check verifies that
each listed sample has exactly one existing file with a `.mzML` extension and
reports missing, duplicate, or invalid entries in the Raw/mzML Files tab.
The built-in table uses `B73_C1.mzML`–`B73_C3.mzML` and
`EA2024_C1.mzML`–`EA2024_C3.mzML`.

For database searching, upload a matching protein FASTA file (for example,
`UP000007305_4577.fasta` for *Zea mays*). ProtVis selects the bundled Sage
executable for the current platform: `windows/sage.exe` on Windows,
`Linux/sage` on Linux, and `macOS/ARM64/sage` or `macOS/Intel/sage` on macOS.
If no matching bundled executable is available, it falls back to `sage` on
`PATH`. The helper `protvis_sage_executable()` reports the selected path. RAW,
mzML, and FASTA bytes are not copied into `ProtVis_dataset`; only paths,
filenames, checks, Sage parameters, and results are recorded for
reproducibility.

### Sage database search

For a Sage-only project, register the built-in or uploaded sample information,
the FASTA, and the validated mzML directory, then click **Project init**. ProtVis
first saves a `Sage_staging` `ProtVis_dataset` containing the sample metadata and
input-file registration, without inventing an expression matrix. Open the
top-level **Sage search** tab to continue. ProtVis uses the registered FASTA and
validated mzML directory, shows the resolved paths and bundled Sage executable,
and exposes the standard Sage parameters.
Click **Run Sage Search** to generate `sage_config.json`, `results.sage.tsv`,
`lfq.tsv`, and `results.json` in the project `Sage_search` directory. The PSM
and LFQ tables, configuration, paths, files, log, and provenance are stored in
`ProtVis_dataset$analysis_results$Sage_database_search`; the stage checkpoint is
written as `Step2_sage_database_search.rda` for downstream workflows.
The search step then updates the Sage staging dataset into the canonical
protein-by-sample `ProtVis_dataset`, converts the Sage LFQ output into the
protein abundance matrix, retains the PSM assay, and preserves staging metadata
and process history. This
matrix becomes the active input for the existing preprocessing,
differential-abundance, enrichment, and other downstream modules. This
staging/finalization behavior is exclusive to the Sage workflow; other data
sources continue to use the existing Project init path.

The PXD065315 metadata source is the [PRIDE project page](https://www.ebi.ac.uk/pride/archive/projects/PXD065315).
Script users can call `load_protvis_builtin_data(source = "DIA-NN")`,
`run_protvis_pipeline()`, `save_protvis_checkpoint()`, and
`export_protvis_dataset()`.

All ProtVis operations use `ProtVis_dataset` as the primary state container.
Each processing node returns a new version with a name such as
`ProtVis_dataset__transformation__log2__v2` and records its parent, method,
parameters, timestamps, results, and errors. Successful and failed nodes are
automatically persisted as an RDS plus a portable export bundle. If no output
directory is supplied, `getwd()` is used; `protvis_output_directory()` exposes
the same resolution rule for scripts and extensions. The Shiny interface
therefore does not require a manual export step.

### Project QC, workflow state, and PSM Explorer

After Project init or Search, open **Project Dashboard** to inspect the active
`ProtVis_dataset`. The dashboard summarizes identified proteins and samples,
matrix completeness, median protein CV, sample-level missingness and intensity,
the dependency-aware workflow graph, provenance events, file fingerprints, and
Sage run-level QC. The **Resume workflow** button continues from the most recent
valid checkpoint/node; a project still in `Sage_staging` is intentionally sent
back to Search instead of inventing downstream quantitative data.

The Search page includes dedicated Sage QC views for run-level IDs, precursor
charge, precursor mass error, q-values, peptide length, missed cleavages, and
retention-time distributions. Open **PSM Explorer** for spectrum-level evidence:
load mzIdentML + MGF files, search/select any PSM, and inspect the matched
PTM-aware b/y fragment ions and annotated spectrum.

### Headless analysis

The Shiny interface and CLI share the same backend. Examples:

```r
# Table-based input
ProtVis::run_protvis_cli(c(
  "--input", "proteins.tsv",
  "--source", "DIA-NN",
  "--output", "results"
))

# Resume an existing project/checkpoint directory
ProtVis::run_protvis_cli(c(
  "--resume", "results",
  "--output", "results"
))
```

For the raw-data route, pass `--fasta`, `--mzml-dir`, and `--sample-info`.
A JSON file supplied with `--config` can provide Sage parameters and downstream
stage parameters. Use `ProtVis::protvis_cli_help()` for the complete command
summary.

------------------------------------------------------------------------

## Installation

### Clean installation from GitHub

Always install ProtVis into an R library. Do not use a Git checkout itself as
an R library directory. For a first installation, use:

``` r
options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
install.packages("pak")
pak::pak("xuebinzhang-lab/ProtVis@dev")
```

When updating an existing installation, especially after an interrupted copy
or installation, start a fresh R session and run the repository's clean
installer. It removes only the exact ProtVis package directory and its install
lock before reinstalling:

``` r
source(
  "https://raw.githubusercontent.com/xuebinzhang-lab/ProtVis/dev/install_ProtVis.R"
)
```

### Optional WGCNA helper

The WGCNA module can use the optional `ShinyWGCNA` helper package. Its upstream GitHub repository is not currently resolvable by `pak` as a hard dependency, so ProtVis treats it as optional and checks for it at runtime.

Install it manually only if you need the WGCNA module:

``` r
install.packages("remotes")
remotes::install_github("ShawnWx2019/WGCNAShinyFun", ref = "master")
```

------------------------------------------------------------------------

## Run ProtVis

``` r
library(ProtVis)
run_ProtVis()
```

------------------------------------------------------------------------

## Recommended workflow

1.  **Create or load a project**
    -   Choose a working directory.
    -   Select a data source in **Project init**.

2.  **Prepare MaxQuant output when applicable**
    -   For MaxQuant, open **Pre-processing → MaxQuant Output Preparation** to upload and filter the MaxQuant output.
    -   Other sources use their dedicated parser-backed import path and do not display this MaxQuant-specific step.
    -   For Proteome Discoverer, Skyline, Mascot, and OpenMS, click the corresponding **Parse ... Output** button.
    -   Upload sample metadata when available. ProtVis expects or infers `sample_id`, `maxquant_id`, and `group` fields for downstream preprocessing.

3.  **Preprocess data**
    -   Continue through **Pre-processing → Correct Noise**, then Data Transformed, Data Imputation, and Data Normalization as needed.
    -   Parser-backed data sources automatically save preprocessing handoff files in the selected working directory.

4.  **Run downstream analysis**
    -   Use DEP analysis, enrichment analysis, GSEA, Pathview, WGCNA, co-enrichment, Venn, and expression profile modules.

5.  **Explore metaproteomics**
    -   Open **Multi-omics → Metaproteomics**.
    -   Use the built-in demo or upload abundance, taxonomy, and function annotation tables.
    -   Generate taxonomy composition, function composition, taxon-function Sankey, and taxon-function heatmap visualizations.

------------------------------------------------------------------------

## Metaproteomics input format

The metaproteomics module accepts three CSV files, all joined by `ProteinID`:

### 1. Abundance table

``` text
ProteinID,Control_1,Control_2,Treatment_1,Treatment_2
MP001,18,20,29,31
MP002,30,27,14,16
```

### 2. Taxonomy annotation table

``` text
ProteinID,Phylum,Genus,Species
MP001,Firmicutes,Faecalibacterium,F. prausnitzii
MP002,Bacteroidota,Bacteroides,B. vulgatus
```

### 3. Function annotation table

``` text
ProteinID,KO,Pathway,COG
MP001,K01689,Butanoate metabolism,Energy production
MP002,K01810,Glycolysis / Gluconeogenesis,Carbohydrate transport
```

The module also includes downloadable demo CSV files from the UI.

------------------------------------------------------------------------

## Requirements

ProtVis is developed in R and relies on CRAN, Bioconductor, and GitHub-hosted R packages. Bioconductor should be configured correctly before installation. A recent R version is recommended for best compatibility.

------------------------------------------------------------------------

## Cookbook

ProtVis cookbook: [https://anhuikylin.github.io/ProtVis-cookbook/](https://anhuikylin.github.io/ProtVis-cookbook/)

------------------------------------------------------------------------

## Getting help

If you encounter installation issues or package dependency conflicts, please check that:

-   Bioconductor is correctly installed.
-   Your R version is compatible with the package dependencies.
-   GitHub-hosted dependencies can be accessed from your network.
-   Optional modules such as WGCNA have their optional helper packages installed when needed.

For additional support, please open an issue in this repository.

------------------------------------------------------------------------

## Citation

If you use ProtVis in your research, please cite the corresponding publication when available.

------------------------------------------------------------------------

## Author

**Fei Liang**<br>
State Key Laboratory of Crop Stress Adaptation and Improvement<br>
Henan Joint International Laboratory for Crop Multi-Omics Research<br>
School of Life Sciences, Henan University<br>
Kaifeng 475004, China

------------------------------------------------------------------------

## License

This project is distributed under the terms specified in the repository license.
