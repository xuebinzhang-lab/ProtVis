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
-   **ProtVis_dataset** standardized object with expression data, sample/variable metadata, annotations, analysis results, provenance, checkpoints, and portable exports.
-   **Recoverable workflow nodes** for QC filtering, transformation, imputation, normalization, dimensionality reduction, differential analysis, enrichment, and network analysis. Failed nodes are recorded and can be retried or resumed without taking down the Shiny session.
-   **Preprocessing workflow handoff** from parser-backed imports into downstream modules such as Correct Noise, Data Transformed, Data Imputation, and Data Normalization.
-   **Protein-level downstream analysis** including DEP analysis, enrichment analysis, GSEA, KEGG/pathway visualization, PPI, WGCNA, co-enrichment, Venn analysis, and expression profiling.
-   **Metaproteomics module** with built-in demo data for abundance, taxonomy, and functional annotations.
-   **Taxonomy-function visualization** including composition plots, Sankey diagrams, and heatmaps for metaproteomics interpretation.
-   **Interactive Shiny interface** for users who prefer GUI-driven analysis and figure generation.
-   **Optional RAW/mzML registration and Sage search preparation** with built-in PXD065315 sample metadata, directory/file consistency checks, and protein FASTA upload.

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
`UP000007305_4577.fasta` for *Zea mays*). On Windows, ProtVis can use the
bundled Sage executable at `inst/extdata/sage/windows/sage.exe`; on Linux or
macOS, install Sage separately and place `sage` on `PATH`. The helper
`protvis_sage_executable()` locates the executable. RAW, mzML, and FASTA bytes
are not copied into `ProtVis_dataset`; only paths, filenames, checks, Sage
parameters, and results are recorded for reproducibility.

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

2.  **Import data**
    -   Use **Data input** to upload the file type matching your selected source.
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
