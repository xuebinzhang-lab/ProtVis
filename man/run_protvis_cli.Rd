\name{run_protvis_cli}
\alias{run_protvis_cli}
\alias{protvis_cli_help}
\title{Run ProtVis in headless mode}
\description{
Execute table-based imports, Sage or FragPipe raw-data searches, checkpoint
resume, downstream analysis, provenance capture, and portable exports without
Shiny.
}
\usage{
run_protvis_cli(args = commandArgs(trailingOnly = TRUE))
protvis_cli_help()
}
\arguments{
\item{args}{Command-line style character vector. Use \code{--engine Sage} or
\code{--engine FragPipe} for raw-data database searching.}
}
\value{
\code{run_protvis_cli()} invisibly returns the final ProtVis_dataset.
\code{protvis_cli_help()} invisibly returns the printed help text.
}
\examples{
\dontrun{
ProtVis::run_protvis_cli(c(
  "--input", "proteins.tsv",
  "--source", "DIA-NN",
  "--output", "results"
))

ProtVis::run_protvis_cli(c(
  "--engine", "FragPipe",
  "--fasta", "proteins.fasta",
  "--spectra-dir", "spectra",
  "--sample-info", "samples.csv",
  "--fragpipe-workflow", "LFQ-MBR.workflow",
  "--output", "results"
))
}
}
