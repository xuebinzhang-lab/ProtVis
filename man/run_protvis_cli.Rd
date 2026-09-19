\name{run_protvis_cli}
\alias{run_protvis_cli}
\alias{protvis_cli_help}
\title{Run ProtVis in headless mode}
\description{
Execute table-based imports, Sage raw-data searches, checkpoint resume,
downstream analysis, provenance capture, and portable exports without Shiny.
}
\usage{
run_protvis_cli(args = commandArgs(trailingOnly = TRUE))
protvis_cli_help()
}
\arguments{
\item{args}{Command-line style character vector.}
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
}
}
