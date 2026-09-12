% Generated manually for the bundled Sage workflow.
\name{sage_search}
\alias{protvis_sage_executable}
\alias{protvis_sage_build_config}
\alias{protvis_read_sage_table}
\alias{run_sage_search}
\alias{sage_search_ui}
\alias{sage_search_server}
\title{Sage database-search workflow}
\description{
  Standardized helpers and Shiny module for running the bundled Sage database
  search engine and retaining its tables and provenance in a
  \code{ProtVis_dataset}.
}
\usage{
protvis_sage_executable(path = NULL)
protvis_sage_build_config(fasta, mzml_paths, output_directory,
  parameters = list())
protvis_read_sage_table(path)
run_sage_search(fasta, mzml_paths, output_directory,
  parameters = list(), sage_path = NULL)
sage_search_ui(id)
sage_search_server(id, shared_state)
}
\arguments{
  \item{path}{Optional Sage executable path.}
  \item{fasta}{Protein FASTA path.}
  \item{mzml_paths}{One or more mzML paths.}
  \item{output_directory}{Directory for Sage outputs.}
  \item{parameters}{Named Sage search parameters.}
  \item{sage_path}{Optional executable override.}
  \item{id}{Shiny module identifier.}
  \item{shared_state}{The shared ProtVis reactive state.}
}
\value{A Sage executable path, configuration, result table, search bundle, or Shiny UI/server result as appropriate.}
