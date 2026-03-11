# ProtVis

## Install

```{r}
install.packages("BiocManager")
BiocManager::install("limma")
BiocManager::install("textshaping")
BiocManager::install("DOSE",force = TRUE)
BiocManager::install("pathview",force = TRUE)
BiocManager::install("DESeq2",force = TRUE)
BiocManager::install("GO.db",force = TRUE)
BiocManager::install("AnnotationDbi",force = TRUE)
BiocManager::install("clusterProfiler",force = TRUE)
install.packages("bio3d")
install.packages("colourpicker")
install.packages("rhandsontable")
install.packages("plotly")
install.packages("shiny")
install.packages("r3dmol")
devtools::install_github("xuebinzhang-lab/ProtVis")
```

# Run

```{r}
library(shiny)
library(plotly)
library(bio3d)
library(r3dmol)
library(ProtVis)
run_ProtVis()
```
