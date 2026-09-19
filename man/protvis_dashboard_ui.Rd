\name{protvis_dashboard_ui}
\alias{protvis_dashboard_ui}
\alias{protvis_dashboard_server}
\title{ProtVis Project QC Dashboard}
\description{
Shiny module for project-wide QC, sample metrics, dependency-aware workflow
state, provenance, file fingerprints, and Sage search QC.
}
\usage{
protvis_dashboard_ui(id)
protvis_dashboard_server(id, shared_state)
}
\arguments{
\item{id}{Shiny module identifier.}
\item{shared_state}{The application reactiveValues project state.}
}
\value{
The UI function returns Shiny UI. The server function returns the active
dataset reactive invisibly.
}
