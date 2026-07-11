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

-   **Multi-source proteomics import** for Raw-style matrices, MaxQuant, Proteome Discoverer, Skyline, Mascot, and OpenMS tabular outputs.
-   **Preprocessing workflow handoff** from parser-backed imports into downstream modules such as Correct Noise, Data Transformed, Data Imputation, and Data Normalization.
-   **Protein-level downstream analysis** including DEP analysis, enrichment analysis, GSEA, KEGG/pathway visualization, PPI, WGCNA, co-enrichment, Venn analysis, and expression profiling.
-   **Metaproteomics module** with built-in demo data for abundance, taxonomy, and functional annotations.
-   **Taxonomy-function visualization** including composition plots, Sankey diagrams, and heatmaps for metaproteomics interpretation.
-   **Interactive Shiny interface** for users who prefer GUI-driven analysis and figure generation.

------------------------------------------------------------------------

## Supported data sources

ProtVis supports the following data-source options from **Project init → Select data source**:

| Data source | Expected input | Notes |
|---|---|---|
| Raw | Generic expression matrix | Use when data are already organized as `ID + sample intensity columns`. |
| MaxQuant | MaxQuant protein group/expression output | Includes MaxQuant-specific unreliable peptide filtering. |
| Proteome Discoverer | Protein or peptide group export (`.xlsx`, `.xls`, `.csv`) | Parses accession/protein ID columns and `Abundance`/`Area` style quantitative columns. |
| Skyline | Skyline report / MSstats-style export (`.csv`, `.tsv`, `.txt`, `.xlsx`, `.xls`) | Supports long reports with `ProteinName`, `FileName`/replicate, and `Area`, or wide protein abundance tables. |
| Mascot | Mascot CSV/export table (`.csv`, `.tsv`, `.txt`, `.xlsx`, `.xls`) | Parses protein accession columns and numeric quantitation columns such as emPAI or intensity. |
| OpenMS | OpenMS consensus / ProteinQuantifier table (`.csv`, `.tsv`, `.txt`, `.xlsx`, `.xls`) | Parses protein accession columns and `intensity`/`abundance`/`area` style columns. |

Parser-backed imports are converted into a common ProtVis expression matrix and sample metadata schema. When a working directory is available, ProtVis writes preprocessing-compatible handoff files so the imported data can continue into the **Pre-processing** menu.

------------------------------------------------------------------------

## Installation

### Install ProtVis from GitHub

``` r
options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
install.packages("pak")
pak::pak("xuebinzhang-lab/ProtVis")
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
