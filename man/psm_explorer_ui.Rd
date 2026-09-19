\name{psm_explorer_ui}
\alias{psm_explorer_ui}
\alias{psm_explorer_server}
\title{ProtVis PSM Explorer}
\description{
Interactive Protein to Peptide to PSM to MS/MS spectrum explorer using
mzIdentML and MGF inputs with PTM-aware b/y fragment annotation.
}
\usage{
psm_explorer_ui(id)
psm_explorer_server(id, shared_state = NULL)
}
\arguments{
\item{id}{Shiny module identifier.}
\item{shared_state}{Optional application reactiveValues project state.}
}
\value{
The UI function returns Shiny UI. The server function returns the selected
PSM result reactive invisibly.
}
