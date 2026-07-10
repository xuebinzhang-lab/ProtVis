# ProtVis

[![](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![](https://img.shields.io/badge/GitHub-ProtVis-blue.svg)](https://github.com/xuebinzhang-lab/ProtVis)
[![](https://img.shields.io/badge/R-Shiny-orange.svg)](https://github.com/xuebinzhang-lab/ProtVis)
[![](https://img.shields.io/badge/platform-all-brightgreen.svg)](https://github.com/xuebinzhang-lab/ProtVis)
[![](https://img.shields.io/badge/license-MIT-green.svg)](https://github.com/xuebinzhang-lab/ProtVis/blob/dev/LICENSE)


<img src="https://github.com/xuebinzhang-lab/ProtVis/blob/dev/app/www/ProtVis_ico.png?raw=true" alt="ProtVis Logo" align="right" width="180"/>

**ProtVis: Simplifying proteomics data exploration with interactive visualization**

ProtVis is an interactive R/Shiny-based platform developed for proteomics data exploration, visualization, and interpretation. It is designed to provide a user-friendly and integrated environment for researchers to analyze proteomics datasets with high-quality, publication-ready visual outputs.

ProtVis supports interactive data exploration and a variety of downstream analyses, helping users efficiently move from raw data inspection to biological interpretation.

<br clear="right"/>

------------------------------------------------------------------------

## Features

-   Interactive proteomics data exploration
-   High-quality visualization for publication and presentation
-   Support for protein-level data interpretation
-   Metaproteomics demo module with taxonomy-function visualization
-   Integrated Shiny-based graphical user interface
-   Easy-to-use workflow for researchers without extensive programming experience

------------------------------------------------------------------------

## Installation

### Install ProtVis from GitHub

``` r
# devtools::install_github("xuebinzhang-lab/ProtVis")
options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
install.packages("pak")
pak::pak("xuebinzhang-lab/ProtVis")
```

The optional WGCNA module uses `ShinyWGCNA`, whose upstream GitHub repository is not currently resolvable by `pak` as an automatic hard dependency. Install it manually only if you need that module:

``` r
remotes::install_github("ShawnWx2019/WGCNAShinyFun", ref = "master")
```

------------------------------------------------------------------------

## Run ProtVis

After installation, load the required libraries and launch ProtVis with:

``` r
library(ProtVis)
run_ProtVis()
```

------------------------------------------------------------------------

## Usage

ProtVis launches as an interactive Shiny application in your R session. Once opened, users can explore proteomics datasets through an intuitive graphical interface and generate informative visualizations for downstream analysis and presentation.

------------------------------------------------------------------------

## Requirements

ProtVis is developed in R and relies on several CRAN and Bioconductor packages.\
It is recommended to use an up-to-date version of R for the best compatibility.

------------------------------------------------------------------------

## Cookbook

ProtVis cookbook: [https://anhuikylin.github.io/ProtVis-cookbook/](https://anhuikylin.github.io/ProtVis-cookbook/)

------------------------------------------------------------------------
## Demo

Demo

------------------------------------------------------------------------
## Getting Help

If you encounter installation issues or package dependency conflicts, please first ensure that:

-   Bioconductor is correctly installed
-   All required packages are installed successfully
-   Your R version is compatible with the package dependencies

For additional support, please open an issue in this repository.

------------------------------------------------------------------------

## Citation

If you use ProtVis in your research, please cite the corresponding publication when available.

------------------------------------------------------------------------

## Author

**Fei Liang & Xiao Wang**\
State Key Laboratory of Crop Stress Adaptation and Improvement\
Henan Joint International Laboratory for Crop Multi-Omics Research\
School of Life Sciences, Henan University\
Kaifeng 475004, China

------------------------------------------------------------------------

## License

This project is distributed under the terms specified in the repository license.
