% Generated manually to keep the checked-in namespace documented.
\name{protvis_preprocessing_defaults}
\alias{protvis_preprocessing_defaults}
\title{Return source-specific preprocessing defaults}
\usage{
protvis_preprocessing_defaults(source = "MaxQuant")
}
\arguments{
\item{source}{Proteomics data source name.}
}
\value{
A named list of preprocessing-stage parameter lists. For MaxQuant, the preset
reproduces the archived workflow: MaxQuant flag filtering, replicate correction,
\code{log2(x * 1e7)}, \code{impute::impute.knn()} with seed 12345, and
sample-median centering followed by the row-wise positive shift.
}
\description{
Returns the preprocessing defaults used by ProtVis for the selected source.
The MaxQuant preset is designed to be reproducible in both the Shiny workflow
and \code{run_protvis_pipeline()} when stage parameters are omitted.
}
