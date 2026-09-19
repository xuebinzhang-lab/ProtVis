\name{predict_plant_mploc}
\alias{predict_plant_mploc}
\alias{print.PlantmPLoc_result}
\title{Predict Plant Protein Subcellular Localization with Plant-mPLoc}
\usage{
predict_plant_mploc(
  sequence,
  id = "query_protein",
  url = .plant_mploc_url,
  ignore_ssl = TRUE,
  timeout = 90,
  verbose = TRUE
)

\method{print}{PlantmPLoc_result}(x, ...)
}
\arguments{
\item{sequence}{Protein sequence using the 20 standard one-letter amino-acid codes.}
\item{id}{Protein identifier used in the submitted FASTA header.}
\item{url}{Plant-mPLoc server URL.}
\item{ignore_ssl}{Whether to disable certificate verification for the legacy server.}
\item{timeout}{Request timeout in seconds.}
\item{verbose}{Whether to report submission progress.}
\item{x}{A \code{PlantmPLoc_result} object.}
\item{...}{Additional print arguments.}
}
\value{
A \code{PlantmPLoc_result} list. The \code{prediction} element contains one
or more predicted subcellular locations.
}
\description{
Submits one complete protein sequence to the public Plant-mPLoc web server
and returns its single- or multi-location prediction.
}
